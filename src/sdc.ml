(** Physical-flow constraints for hand-instantiated islands, and a check that
    they survived.

    Yosys honours the [keep] attribute {!Cell} puts on every hand-instantiated
    cell, but OpenROAD and LibreLane ignore netlist attributes: without
    [set_dont_touch], [repair_design] and [repair_timing] may resize ring
    inverters, buffer ring nodes or delete delay buffers. Nothing constrains
    placement or routing either.

    {2 What is protected}

    [create] reads a flattened Hardcaml circuit. Every instance carrying
    {!Cell.attributes}' [dont_touch] is protected, and so is every register
    carrying it (the first sampling stage of {!Tdc}, or of {!Ring.S.Clk_sampler}
    and [Pin_capture] without [first_stage] cells). Protected nets are those
    driven by a protected instance or register, plus registered {e fork}
    nets: inputs whose branches race each other, e.g. a captured pin that
    feeds both the merge NAND and its snapshot buffer. Net names are the
    circuit's ({!Cell} names every output [<instance>__<pin>]).

    A kept register becomes a flop with a generated instance name in
    synthesis; only its output net is named. {!dont_touch_list} resolves those
    instances (and unnamed fork nets) from a synthesised netlist; prefer cell
    first stages where a structure offers them.

    {2 LibreLane}

    LibreLane applies [RSZ_DONT_TOUCH_RX] (a Tcl regexp matched against every
    net and instance name) and [RSZ_DONT_TOUCH_LIST] only around its resizer
    steps (design repair after global placement and routing, timing repair
    after CTS and routing, and global placement); clock tree synthesis and
    antenna repair do not see them. Hence island clocks are only in the
    [Signoff] SDC ([SIGNOFF_SDC_FILE]): in the [Pnr] SDC ([PNR_SDC_FILE]) a
    clock would make CTS build a tree, and clone inverters, on a ring or bank
    clock net. During implementation, paths inside an island are then
    unconstrained, and the resizer leaves them alone; signoff STA checks them.

    The SDC text is a fragment to append to the design's own SDC, after the
    system clock is created.

    {2 Hierarchy}

    Pass a flattened circuit. [prefix] is prepended to every instance and net
    name: the path yosys gives the island when it flattens a hierarchical
    design ([<instance>.]; LibreLane's default), or [""] for a design
    flattened by Hardcaml. *)

open! Base
open Hardcaml

module Endpoint = struct
  type t =
    { instance : string
    ; pin : string
    }
  [@@deriving sexp_of, compare, equal]

  let to_string t = t.instance ^ "/" ^ t.pin
end

module Driver = struct
  type t =
    | Cell of Endpoint.t (** a protected instance's output *)
    | Register of string (** a kept register, by name *)
    | Constant of bool
    | Other (** synthesised logic or an input port *)
  [@@deriving sexp_of]
end

module Net = struct
  type t =
    { names : string list (** circuit names; bus bits as [name[i]] *)
    ; driver : Driver.t
    ; sinks : Endpoint.t list (** protected input pins *)
    ; other_sinks : bool (** also feeds logic outside the island, or a port *)
    ; fork : bool
    }
  [@@deriving sexp_of]

  let is_protected t =
    t.fork
    ||
    match t.driver with
    | Cell _ | Register _ -> true
    | Constant _ | Other -> false
  ;;
end

module Instance = struct
  type t =
    { name : string
    ; cell : string
    ; pins : (string * int) list (** pin, index into [nets] *)
    }
  [@@deriving sexp_of]
end

module Register = struct
  type t =
    { name : string
    ; q : int
    ; d : int
    }
  [@@deriving sexp_of]
end

module Clock = struct
  type t =
    { name : string
    ; source : Endpoint.t
    ; period_ns : float
    ; waveform : (float * float) option
    ; group : string
    }
  [@@deriving sexp_of]
end

module Stage = struct
  type t =
    | Pnr
    | Signoff
end

type t =
  { prefix : string
  ; instances : Instance.t list
  ; registers : Register.t list
  ; nets : Net.t array
  ; loop_breaks : Endpoint.t list
  ; clocks : Clock.t list
  ; false_paths_through : Endpoint.t list
  }

let is_dont_touch s =
  List.exists (Signal.Type.get_attributes s) ~f:(fun a ->
    String.equal (Rtl_attribute.name a) "dont_touch")
;;

let create ?(prefix = "") (circuit : Circuit.t) =
  let graph = Circuit.signal_graph circuit in
  let signals =
    Signal_graph.fold graph ~init:[] ~f:(fun acc s -> s :: acc)
    |> List.rev
    |> List.filter ~f:(fun s -> not (Signal.Type.is_empty s))
  in
  (* Nets: one per signal bit, joined through wires, selects and concatenations. *)
  let base = Hashtbl.create (module Signal.Type.Uid) in
  let nraw = ref 0 in
  List.iter signals ~f:(fun s ->
    Hashtbl.set base ~key:(Signal.uid s) ~data:!nraw;
    nraw := !nraw + Signal.width s);
  let raw s i = Hashtbl.find_exn base (Signal.uid s) + i in
  let parent = Array.init !nraw ~f:Fn.id in
  let rec find i =
    let p = parent.(i) in
    if p = i
    then i
    else (
      let r = find p in
      parent.(i) <- r;
      r)
  in
  let union a b =
    let a = find a
    and b = find b in
    if a <> b then parent.(a) <- b
  in
  List.iter signals ~f:(fun s ->
    match s with
    | Signal.Type.Wire { driver = Some d; _ } ->
      for i = 0 to Signal.width s - 1 do
        union (raw s i) (raw d i)
      done
    | Select { arg; low; _ } ->
      for i = 0 to Signal.width s - 1 do
        union (raw s i) (raw arg (low + i))
      done
    | Cat { args; _ } ->
      let off = ref 0 in
      List.iter (List.rev args) ~f:(fun a ->
        for i = 0 to Signal.width a - 1 do
          union (raw s (!off + i)) (raw a i)
        done;
        off := !off + Signal.width a)
    | _ -> ());
  let index = Array.create ~len:!nraw (-1) in
  let count = ref 0 in
  for i = 0 to !nraw - 1 do
    let r = find i in
    if index.(r) < 0
    then (
      index.(r) <- !count;
      Int.incr count)
  done;
  let net s i = index.(find (raw s i)) in
  let names = Array.create ~len:!count [] in
  let driver = Array.create ~len:!count Driver.Other in
  let sinks = Array.create ~len:!count [] in
  let other = Array.create ~len:!count false in
  List.iter signals ~f:(fun s ->
    let w = Signal.width s in
    List.iter (Signal.names s) ~f:(fun nm ->
      for i = 0 to w - 1 do
        let n = net s i in
        let nm = if w = 1 then nm else Printf.sprintf "%s[%d]" nm i in
        if not (List.mem names.(n) nm ~equal:String.equal) then names.(n) <- nm :: names.(n)
      done));
  let mark_other s =
    for i = 0 to Signal.width s - 1 do
      other.(net s i) <- true
    done
  in
  List.iter (Circuit.outputs circuit) ~f:mark_other;
  let instances = Queue.create () in
  let registers = Queue.create () in
  List.iter signals ~f:(fun s ->
    match s with
    | Signal.Type.Wire _ | Select _ | Cat _ | Empty -> ()
    | Const { constant; _ } ->
      let bits = Bits.to_string constant (* msb first *) in
      String.iteri (String.rev bits) ~f:(fun i b -> driver.(net s i) <- Constant (Char.equal b '1'))
    | Inst { instantiation = inst; _ } when is_dont_touch s ->
      let pins =
        List.map inst.inputs ~f:(fun i ->
          if Signal.width i.input_signal <> 1
          then raise_s [%message "Sdc: cell pins are 1 bit" inst.instance_label i.name];
          let n = net i.input_signal 0 in
          sinks.(n) <- { Endpoint.instance = inst.instance_label; pin = i.name } :: sinks.(n);
          i.name, n)
        @ List.map inst.outputs ~f:(fun o ->
          let n = net s o.output_low_index in
          driver.(n) <- Cell { instance = inst.instance_label; pin = o.name };
          o.name, n)
      in
      Queue.enqueue
        instances
        { Instance.name = inst.instance_label; cell = inst.circuit_name; pins }
    | Reg { d; _ } when is_dont_touch s ->
      let name =
        match Signal.names s with
        | nm :: _ -> nm
        | [] -> raise_s [%message "Sdc: kept register without a name"]
      in
      if Signal.width s <> 1 then raise_s [%message "Sdc: kept registers must be 1 bit" (name : string)];
      driver.(net s 0) <- Register name;
      Queue.enqueue registers { Register.name; q = net s 0; d = net d 0 };
      Signal.Type.Deps.iter s ~f:mark_other
    | _ -> Signal.Type.Deps.iter s ~f:mark_other);
  let nets =
    Array.init !count ~f:(fun n ->
      { Net.names = List.sort names.(n) ~compare:String.compare
      ; driver = driver.(n)
      ; sinks = List.sort sinks.(n) ~compare:Endpoint.compare
      ; other_sinks = other.(n)
      ; fork = false
      })
  in
  { prefix
  ; instances = Queue.to_list instances |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
  ; registers = Queue.to_list registers
  ; nets
  ; loop_breaks = []
  ; clocks = []
  ; false_paths_through = []
  }
;;

let instances t = t.instances

let find_instance t name =
  List.find t.instances ~f:(fun (i : Instance.t) -> String.equal i.name name)
;;

let pin_net t (e : Endpoint.t) =
  match find_instance t e.instance with
  | None -> raise_s [%message "Sdc: no protected instance" (e.instance : string)]
  | Some i ->
    (match List.Assoc.find i.pins e.pin ~equal:String.equal with
     | Some n -> n
     | None -> raise_s [%message "Sdc: no such pin" (e : Endpoint.t)])
;;

let is_input t (e : Endpoint.t) =
  List.exists t.nets.(pin_net t e).sinks ~f:(Endpoint.equal e)
;;

(* --- Registration --- *)

let loop_break t (e : Endpoint.t) =
  if not (is_input t e) then raise_s [%message "Sdc.loop_break: not an input pin" (e : Endpoint.t)];
  { t with loop_breaks = t.loop_breaks @ [ e ] }
;;

let fork t (e : Endpoint.t) =
  if not (is_input t e) then raise_s [%message "Sdc.fork: not an input pin" (e : Endpoint.t)];
  let n = pin_net t e in
  let nets = Array.copy t.nets in
  nets.(n) <- { (nets.(n)) with fork = true };
  { t with nets }
;;

let clock t ~name ~source ?waveform ~period_ns ~group () =
  ignore (pin_net t source : int);
  if List.exists t.clocks ~f:(fun c -> String.equal c.name name)
  then raise_s [%message "Sdc.clock: duplicate name" (name : string)];
  { t with clocks = t.clocks @ [ { Clock.name; source; period_ns; waveform; group } ] }
;;

let false_path_through t (e : Endpoint.t) =
  ignore (pin_net t e : int);
  { t with false_paths_through = t.false_paths_through @ [ e ] }
;;

let output_pin t instance =
  match find_instance t instance with
  | None -> raise_s [%message "Sdc: no protected instance" (instance : string)]
  | Some i ->
    (match
       List.filter i.pins ~f:(fun (_, n) ->
         match t.nets.(n).driver with
         | Cell e -> String.equal e.instance instance
         | _ -> false)
     with
     | [ (pin, _) ] -> { Endpoint.instance; pin }
     | _ -> raise_s [%message "Sdc: expected one output" (instance : string)])
;;

let input_pins t instance =
  match find_instance t instance with
  | None -> raise_s [%message "Sdc: no protected instance" (instance : string)]
  | Some i ->
    List.filter_map i.pins ~f:(fun (pin, _) ->
      let e = { Endpoint.instance; pin } in
      Option.some_if (is_input t e) e)
;;

let ring t ~name ~loop_breaks ~period_ns =
  let t = List.fold loop_breaks ~init:t ~f:loop_break in
  let half = period_ns /. 2. in
  match find_instance t (name ^ "_clkinv_a2") with
  | Some _ ->
    (* flop-cell counters: A on node 0 through two inverters, B through one *)
    let t =
      clock t ~name:(name ^ "_a") ~source:(output_pin t (name ^ "_clkinv_a2")) ~period_ns ~group:name ()
    in
    clock
      t
      ~name:(name ^ "_b")
      ~source:(output_pin t (name ^ "_clkinv_b"))
      ~waveform:(half, period_ns)
      ~period_ns
      ~group:name
      ()
  | None ->
    (* register counters on both edges of node 0 *)
    clock t ~name ~source:(output_pin t (name ^ "_nand")) ~period_ns ~group:name ()
;;

let tdc t ~name =
  let stage0 = List.hd_exn (input_pins t (name ^ "_stage0")) in
  false_path_through (fork t stage0) stage0
;;

let pin_capture t ~name ~pins ~min_edge_spacing_ns =
  let s = min_edge_spacing_ns in
  let p = 2. *. s in
  let n fmt = Printf.ksprintf (fun x -> name ^ "_" ^ x) fmt in
  let t =
    List.fold (List.init pins ~f:Fn.id) ~init:t ~f:(fun t i ->
      (* the pin net: snapshot buffer, merge NAND, delay line, edge counters *)
      let t = fork t (List.hd_exn (input_pins t (n "dpin%d" i))) in
      let pin_net_i = pin_net t (List.hd_exn (input_pins t (n "dpin%d" i))) in
      let ecr_clk =
        List.find_exn (input_pins t (n "ecr%d" i)) ~f:(fun e -> pin_net t e = pin_net_i)
      in
      let t = clock t ~name:(n "ecr%d" i) ~source:ecr_clk ~period_ns:p ~group:name () in
      clock
        t
        ~name:(n "ecf%d" i)
        ~source:(output_pin t (n "ecclk%d" i))
        ~waveform:(s, p)
        ~period_ns:p
        ~group:name
        ())
  in
  let t = clock t ~name:(n "clkr") ~source:(output_pin t (n "clkr")) ~period_ns:p ~group:name () in
  clock t ~name:(n "clkf") ~source:(output_pin t (n "clkf")) ~waveform:(s, p) ~period_ns:p ~group:name ()
;;

(* --- Outputs --- *)

let protected_nets t = Array.to_list t.nets |> List.filter ~f:Net.is_protected

let net_names t =
  List.concat_map (protected_nets t) ~f:(fun n -> List.map n.names ~f:(fun nm -> t.prefix ^ nm))
  |> List.dedup_and_sort ~compare:String.compare
;;

let instance_names t = List.map t.instances ~f:(fun i -> t.prefix ^ i.name)

(* A name as a Tcl ARE, matching OpenROAD's escaped bus brackets too. *)
let rx_of_name nm =
  String.concat_map nm ~f:(fun c ->
    match c with
    | '[' -> "\\\\?\\["
    | ']' -> "\\\\?\\]"
    | c when Char.is_alphanum c || Char.equal c '_' -> String.of_char c
    | c -> "\\" ^ String.of_char c)
;;

let rsz_dont_touch_rx t =
  let names =
    List.map t.instances ~f:(fun i -> i.name)
    @ List.concat_map (protected_nets t) ~f:(fun n -> n.names)
    |> List.dedup_and_sort ~compare:String.compare
  in
  Printf.sprintf
    "^%s(%s)$"
    (rx_of_name t.prefix)
    (String.concat ~sep:"|" (List.map names ~f:rx_of_name))
;;

let tcl_name nm = "{" ^ nm ^ "}"

let sdc t ~(stage : Stage.t) ~system_clock =
  let b = Buffer.create 1024 in
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string b (s ^ "\n")) fmt in
  let pin (e : Endpoint.t) = tcl_name (t.prefix ^ Endpoint.to_string e) in
  line
    "# hardcaml_clockless islands (%s): append after the system clock is created."
    (match stage with
     | Pnr -> "PnR"
     | Signoff -> "signoff");
  List.iter t.loop_breaks ~f:(fun e -> line "set_disable_timing [get_pins %s]" (pin e));
  List.iter t.false_paths_through ~f:(fun e -> line "set_false_path -through [get_pins %s]" (pin e));
  (match stage with
   | Pnr ->
     if not (List.is_empty t.clocks)
     then
       line
         "# Island clocks (%s) are in the signoff SDC only: CTS ignores dont_touch."
         (String.concat ~sep:" " (List.map t.clocks ~f:(fun c -> c.name)))
   | Signoff ->
     List.iter t.clocks ~f:(fun c ->
       line
         "create_clock -name %s -period %.4f%s [get_pins %s]"
         c.name
         c.period_ns
         (match c.waveform with
          | None -> ""
          | Some (r, f) -> Printf.sprintf " -waveform {%.4f %.4f}" r f)
         (pin c.source));
     let groups =
       List.fold t.clocks ~init:[] ~f:(fun acc c ->
         if List.mem acc c.group ~equal:String.equal then acc else acc @ [ c.group ])
     in
     if not (List.is_empty groups)
     then
       line
         "set_clock_groups -asynchronous -group [get_clocks %s] %s"
         (tcl_name system_clock)
         (String.concat
            ~sep:" "
            (List.map groups ~f:(fun g ->
               Printf.sprintf
                 "-group [get_clocks {%s}]"
                 (String.concat
                    ~sep:" "
                    (List.filter_map t.clocks ~f:(fun c ->
                       Option.some_if (String.equal c.group g) c.name)))))));
  Buffer.contents b
;;

(* --- Netlist check --- *)

(* Pin direction from the Liberty file. A protected instance that synthesis or
   P&R swapped for a cell the library does not know is read as the expected
   cell, so that one error reports it. *)
let direction t (library : Cell_library.t) (i : Netlist.Instance.t) pin =
  let cell =
    match Cell_library.find library i.cell with
    | Some c -> Some c
    | None ->
      Option.bind
        (if String.is_prefix i.name ~prefix:t.prefix
         then find_instance t (String.drop_prefix i.name (String.length t.prefix))
         else None)
        ~f:(fun (e : Instance.t) -> Cell_library.find library e.cell)
  in
  match cell with
  | None -> None
  | Some c -> Some (Option.map (Cell_library.Cell.pin c pin) ~f:(fun p -> p.direction))
;;

(* Drivers and loads of every netlist net, by pin direction. Pins the library
   does not list on a cell it knows (power) are ignored; every pin of a cell it
   does not know counts as a load. *)
let drivers_and_loads t library (nl : Netlist.t) =
  let drivers = Array.create ~len:(Netlist.num_nets nl) [] in
  let loads = Array.create ~len:(Netlist.num_nets nl) [] in
  List.iter (Netlist.instances nl) ~f:(fun i ->
    List.iter i.pins ~f:(fun (pin, c) ->
      match c with
      | Open -> ()
      | Net n ->
        (match direction t library i pin with
         | Some (Some Output) -> drivers.(n) <- (i, pin) :: drivers.(n)
         | Some (Some Input) | None -> loads.(n) <- (i, pin) :: loads.(n)
         | Some (Some (Inout | Internal)) | Some None -> ())));
  drivers, loads
;;

let check ?passive t ~(library : Cell_library.t) (nl : Netlist.t) =
  let errors = Queue.create () in
  let err fmt = Printf.ksprintf (Queue.enqueue errors) fmt in
  let p nm = t.prefix ^ nm in
  let lib_cell name = Cell_library.find library name in
  let passive =
    match passive with
    | Some f -> f
    | None ->
      fun cell ->
        (match lib_cell cell with
         | Some c -> List.is_empty (Cell_library.Cell.outputs c)
         | None -> false)
  in
  let direction i pin = Option.join (direction t library i pin) in
  let drivers, loads = drivers_and_loads t library nl in
  let net_str n =
    match Netlist.net_names nl n with
    | [] -> Printf.sprintf "<net %d>" n
    | names -> String.concat ~sep:"," names
  in
  (* protected instances *)
  let found = Hashtbl.create (module String) in
  List.iter t.instances ~f:(fun e ->
    match Netlist.find_instance nl (p e.name) with
    | None -> err "missing instance %s (%s)" (p e.name) e.cell
    | Some i ->
      if not (String.equal i.cell e.cell)
      then err "instance %s is %s, expected %s" (p e.name) i.cell e.cell;
      Hashtbl.set found ~key:e.name ~data:i);
  let netlist_net (e : Endpoint.t) =
    match Hashtbl.find found e.instance with
    | None -> None
    | Some i ->
      (match List.Assoc.find i.pins e.pin ~equal:String.equal with
       | Some (Net n) -> Some n
       | Some Open | None ->
         err "%s is not connected" (p (Endpoint.to_string e));
         None)
  in
  let protected_instance (i : Netlist.Instance.t) =
    String.is_prefix i.name ~prefix:t.prefix
    && Hashtbl.mem found (String.drop_prefix i.name (String.length t.prefix))
  in
  let expected_name (net : Net.t) n =
    let have = Netlist.net_names nl n in
    if not (List.exists net.names ~f:(fun nm -> List.mem have (p nm) ~equal:String.equal))
    then
      err
        "net %s is named %s in the netlist: the dont-touch names do not match it"
        (p (List.hd_exn net.names))
        (net_str n)
  in
  (* trace back through unprotected buffers *)
  let rec root n depth =
    if depth > 64 || n = Netlist.const0 || n = Netlist.const1
    then n
    else (
      match drivers.(n) with
      | [ (i, _) ] when not (protected_instance i) ->
        (match lib_cell i.cell with
         | Some c when Cell_library.Cell.is_buffer c ->
           (match List.find_map i.pins ~f:(fun (pin, c) ->
                    match c, direction i pin with
                    | Net m, Some Input -> Some m
                    | _ -> None)
            with
            | Some m -> root m (depth + 1)
            | None -> n)
         | _ -> n)
      | _ -> n)
  in
  let tie_value n =
    if n = Netlist.const0
    then Some false
    else if n = Netlist.const1
    then Some true
    else (
      match drivers.(n) with
      | [ (i, pin) ] ->
        (match lib_cell i.cell with
         | Some c when Cell_library.Kind.equal c.kind Constant ->
           (match Cell_library.Cell.pin c pin with
            | Some { function_ = Some (Const b); _ } -> Some b
            | _ -> None)
         | _ -> None)
      | _ -> None)
  in
  Array.iteri t.nets ~f:(fun _ net ->
    let label = match net.names with nm :: _ -> p nm | [] -> "<unnamed>" in
    let endpoints =
      (match net.driver with
       | Cell e -> [ e ]
       | _ -> [])
      @ net.sinks
    in
    let mapped =
      List.filter_map endpoints ~f:(fun e -> Option.map (netlist_net e) ~f:(fun n -> e, n))
    in
    if Net.is_protected net
    then (
      let ns = List.map mapped ~f:snd |> List.dedup_and_sort ~compare:Int.compare in
      let ns =
        match ns, net.driver with
        | [], Register r ->
          (match Netlist.find_net nl (p r) with
           | Some n -> [ n ]
           | None ->
             err "kept register %s: no net of that name" (p r);
             [])
        | _ -> ns
      in
      match ns with
      | [] -> ()
      | _ :: _ :: _ ->
        err
          "net %s is split: %s"
          label
          (String.concat
             ~sep:"; "
             (List.map mapped ~f:(fun (e, n) -> Endpoint.to_string e ^ " on " ^ net_str n)))
      | [ n ] ->
        if not (List.is_empty net.names) then expected_name net n;
        (match drivers.(n), net.driver with
         | [ _ ], _ -> ()
         | [], (Constant _ | Other) -> () (* an input port *)
         | ds, _ ->
           err
             "net %s has %d drivers: %s"
             label
             (List.length ds)
             (String.concat ~sep:", " (List.map ds ~f:(fun (i, pin) -> i.name ^ "/" ^ pin))));
        (match net.driver with
         | Register r ->
           (match drivers.(n) with
            | [ (i, _) ] ->
              (match lib_cell i.cell with
               | Some c when Cell_library.Kind.equal c.kind Flop -> ()
               | _ -> err "kept register %s is driven by %s (%s), not a flop" (p r) i.name i.cell);
              (* its D must come from the expected net, when that net is protected *)
              let reg = List.find_exn t.registers ~f:(fun x -> String.equal x.name r) in
              let dnet = t.nets.(reg.d) in
              if Net.is_protected dnet
              then (
                let d_ends =
                  (match dnet.driver with Cell e -> [ e ] | _ -> []) @ dnet.sinks
                in
                match List.find_map d_ends ~f:netlist_net with
                | None -> ()
                | Some dn ->
                  if not
                       (List.exists i.pins ~f:(fun (pin, c) ->
                          Netlist.Conn.equal c (Net dn)
                          && Poly.equal (direction i pin) (Some Cell_library.Direction.Input)))
                  then err "kept register %s: D is not on net %s" (p r) (net_str dn))
            | _ -> ())
         | _ -> ());
        if not net.other_sinks
        then (
          let expected =
            List.filter_map net.sinks ~f:(fun e ->
              Option.map (Hashtbl.find found e.instance) ~f:(fun i -> i.name, e.pin))
          in
          List.iter loads.(n) ~f:(fun (i, pin) ->
            if not
                 (List.mem expected (i.name, pin) ~equal:[%equal: string * string]
                  || passive i.cell)
            then err "net %s has an extra load %s/%s (%s)" label i.name pin i.cell)))
    else (
      (* inputs from outside the island: sinks may be rebuffered, but must
         keep a common source; constants may become tie cells *)
      let roots = List.map mapped ~f:(fun (e, n) -> e, root n 0) in
      (match net.driver with
       | Constant b ->
         List.iter roots ~f:(fun (e, r) ->
           match tie_value r with
           | Some v when Bool.equal v b -> ()
           | _ ->
             err
               "%s should be constant %d, is on %s"
               (p (Endpoint.to_string e))
               (Bool.to_int b)
               (net_str r))
       | _ ->
         (match List.dedup_and_sort (List.map roots ~f:snd) ~compare:Int.compare with
          | [] | [ _ ] -> ()
          | _ ->
            err
              "net %s: its island loads no longer share a source: %s"
              label
              (String.concat
                 ~sep:"; "
                 (List.map roots ~f:(fun (e, r) -> Endpoint.to_string e ^ " from " ^ net_str r))));
         (* anchor: a surviving circuit name must be that source *)
         match roots with
         | [] -> ()
         | (_, r) :: _ ->
           List.iter net.names ~f:(fun nm ->
             match Netlist.find_net nl (p nm) with
             | Some m when root m 0 <> r ->
               err "net %s: island loads are driven from %s, not from %s" label (net_str r) (p nm)
             | _ -> ()))));
  Queue.to_list errors
;;

let dont_touch_list t ~library (nl : Netlist.t) =
  let p nm = t.prefix ^ nm in
  let drivers, _ = drivers_and_loads t library nl in
  let insts =
    List.filter_map t.instances ~f:(fun i ->
      Option.map (Netlist.find_instance nl (p i.name)) ~f:(fun (x : Netlist.Instance.t) -> x.name))
  in
  let net_of_endpoint (e : Endpoint.t) =
    match Netlist.find_instance nl (p e.instance) with
    | None -> None
    | Some i ->
      (match List.Assoc.find i.pins e.pin ~equal:String.equal with
       | Some (Net n) -> Some n
       | Some Open | None -> None)
  in
  let resolved =
    List.concat_map (protected_nets t) ~f:(fun net ->
      let n =
        match net.driver, net.sinks with
        | Cell e, _ -> net_of_endpoint e
        | _, e :: _ -> net_of_endpoint e
        | _ -> List.find_map net.names ~f:(fun nm -> Netlist.find_net nl (p nm))
      in
      match n with
      | None -> []
      | Some n ->
        let flop =
          match net.driver, drivers.(n) with
          | Register _, [ ((i : Netlist.Instance.t), _) ] -> [ i.name ]
          | _ -> []
        in
        Netlist.net_names nl n @ flop)
  in
  List.dedup_and_sort ~compare:String.compare (insts @ resolved)
;;
