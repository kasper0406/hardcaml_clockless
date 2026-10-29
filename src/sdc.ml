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
    carrying it (the first sampling stage of {!Tdc}, {!Ring.S.Clk_sampler}
    and [Pin_capture] without [first_stage] cells). Protected nets are those
    driven by a protected instance or register (but not outputs of
    protected instances that feed only synthesised logic, e.g. a first
    sampling stage: hold repair must be able to buffer them, and a
    protected net is never repaired), registered {e fork} nets
    (inputs whose branches race each other, e.g. a captured pin that feeds
    both the merge NAND and its snapshot buffer) and {e boundary} nets:
    nets from synthesised logic into a protected instance (a mux select, a
    flop reset). OpenROAD cannot buffer a net with a dont-touch load (the
    load pin cannot move to the new net: [repair_design] stops with
    RSZ-3006), so those nets must not be repaired at all; keep their fanout
    small (the PDK's max fanout) or build their fanout from cells. Inputs
    of the circuit are left alone: the clock is the parent's and CTS builds
    its tree. Net names are the circuit's ({!Cell} names every output
    [<instance>__<pin>]); synthesis may rename boundary nets, which
    {!dont_touch_list} resolves.

    A kept register becomes a flop with a generated instance name in
    synthesis; only its output net is named. {!dont_touch_list} resolves those
    instances (and renamed nets) from a synthesised netlist; prefer cell
    first stages where a structure offers them.

    {2 LibreLane}

    LibreLane applies [RSZ_DONT_TOUCH_RX] (a Tcl regexp matched against every
    net and instance name; {!rsz_dont_touch_rx} before synthesis,
    {!dont_touch_rx} of {!dont_touch_list} after) only around its resizer
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
    | Input (** an input port of the circuit *)
    | Other (** synthesised logic *)
  [@@deriving sexp_of]
end

module Net = struct
  type t =
    { names : string list (** circuit names; bus bits as [name[i]] *)
    ; driver : Driver.t
    ; sinks : Endpoint.t list (** protected input pins *)
    ; logic_sinks : bool (** also feeds logic outside the island *)
    ; ports : string list (** circuit outputs on it *)
    ; fork : bool
    ; boundary : bool (** from synthesised logic into a protected instance *)
    ; kept_sink : bool (** feeds a kept register (D, clock or reset) *)
    }
  [@@deriving sexp_of]

  let is_protected t =
    t.fork
    || t.boundary
    ||
    match t.driver with
    | Register _ -> true
    | Cell _ ->
      (* not an exit into synthesised logic alone (a first sampling stage's
         output): the resizer must be free to fix hold there *)
      not (List.is_empty t.sinks && List.is_empty t.ports && not t.kept_sink)
    | Constant _ | Input | Other -> false
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
    ; clock : int
    ; rising : bool
    ; has_reset : bool
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

(** [set_max_delay delay_ns -from from -to to_]: [from] are register
    instances (none: any startpoint), [to_] internal pins, which STA makes
    path endpoints (and, without [-probe], breaks paths through). *)
module Max_delay = struct
  type t =
    { delay_ns : float
    ; from : string list
    ; to_ : Endpoint.t list
    }
  [@@deriving sexp_of]
end

(** [set_false_path -from from -to [all_registers]] or [[all_outputs]]:
    [from] are register instances. *)
module False_path_from = struct
  type to_ =
    | All_registers
    | All_outputs
  [@@deriving sexp_of]

  type t =
    { from : string list
    ; to_ : to_
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
  ; isolators : (string * int * int) list
  (** {!Cell.isolate} buffers: instance, input net, output net *)
  ; nets : Net.t array
  ; loop_breaks : Endpoint.t list
  ; clocks : Clock.t list
  ; false_paths_through : Endpoint.t list
  ; false_paths_from : False_path_from.t list
  ; max_delays : Max_delay.t list
  }

let has_attribute s name =
  List.exists (Signal.Type.get_attributes s) ~f:(fun a -> String.equal (Rtl_attribute.name a) name)
;;

let is_dont_touch s = has_attribute s "dont_touch"

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
  let logic = Array.create ~len:!count false in
  let ports = Array.create ~len:!count [] in
  let kept_sink = Array.create ~len:!count false in
  List.iter signals ~f:(fun s ->
    let w = Signal.width s in
    List.iter (Signal.names s) ~f:(fun nm ->
      for i = 0 to w - 1 do
        let n = net s i in
        let nm = if w = 1 then nm else Printf.sprintf "%s[%d]" nm i in
        if not (List.mem names.(n) nm ~equal:String.equal) then names.(n) <- nm :: names.(n)
      done));
  let mark_logic s =
    for i = 0 to Signal.width s - 1 do
      logic.(net s i) <- true
    done
  in
  List.iter (Circuit.outputs circuit) ~f:(fun o ->
    let w = Signal.width o in
    List.iter (List.take (Signal.names o) 1) ~f:(fun nm ->
      for i = 0 to w - 1 do
        let n = net o i in
        ports.(n) <- (if w = 1 then nm else Printf.sprintf "%s[%d]" nm i) :: ports.(n)
      done));
  (* An island inside a hierarchy (a prefix) shares its inputs with the
     parent, whose loads on them are its business, like those on outputs. *)
  List.iter (Circuit.inputs circuit) ~f:(fun i ->
    for b = 0 to Signal.width i - 1 do
      driver.(net i b) <- Input;
      if not (String.is_empty prefix) then logic.(net i b) <- true
    done);
  let instances = Queue.create () in
  let registers = Queue.create () in
  let isolators = Queue.create () in
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
    | Inst { instantiation = { inputs = [ i ]; outputs = [ o ]; instance_label; _ }; _ }
      when has_attribute s "keep" ->
      (* an isolating buffer (Cell.isolate): synthesised logic to the
         resizer, but checked to sit between its two nets *)
      Queue.enqueue isolators (instance_label, net i.input_signal 0, net s o.output_low_index);
      mark_logic i.input_signal
    | Reg { d; register = r; _ } when is_dont_touch s ->
      let name =
        match Signal.names s with
        | nm :: _ -> nm
        | [] -> raise_s [%message "Sdc: kept register without a name"]
      in
      if Signal.width s <> 1
      then raise_s [%message "Sdc: kept registers must be 1 bit" (name : string)];
      if Option.is_some r.enable || Option.is_some r.clear
      then raise_s [%message "Sdc: kept registers must be plain flops (no enable or clear)" name];
      driver.(net s 0) <- Register name;
      kept_sink.(net d 0) <- true;
      kept_sink.(net r.clock.clock 0) <- true;
      Option.iter r.reset ~f:(fun x -> kept_sink.(net x.reset 0) <- true);
      Queue.enqueue
        registers
        { Register.name
        ; q = net s 0
        ; d = net d 0
        ; clock = net r.clock.clock 0
        ; rising =
            (match r.clock.clock_edge with
             | Rising -> true
             | Falling -> false)
        ; has_reset = Option.is_some r.reset
        };
      (* its D is checked as the flop's data pin; clock and reset are boundary *)
      mark_logic r.clock.clock;
      Option.iter r.reset ~f:(fun x -> mark_logic x.reset)
    | _ -> Signal.Type.Deps.iter s ~f:mark_logic);
  let nets =
    Array.init !count ~f:(fun n ->
      { Net.names = List.sort names.(n) ~compare:String.compare
      ; driver = driver.(n)
      ; sinks = List.sort sinks.(n) ~compare:Endpoint.compare
      ; logic_sinks = logic.(n)
      ; ports = List.sort ports.(n) ~compare:String.compare
      ; fork = false
      ; kept_sink = kept_sink.(n)
      ; boundary =
          (match driver.(n) with
           | Other -> not (List.is_empty sinks.(n))
           | Cell _ | Register _ | Constant _ | Input -> false)
      })
  in
  { prefix
  ; instances = Queue.to_list instances |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
  ; registers = Queue.to_list registers
  ; isolators = Queue.to_list isolators
  ; nets
  ; loop_breaks = []
  ; clocks = []
  ; false_paths_through = []
  ; false_paths_from = []
  ; max_delays = []
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
  List.iter [ name; group ] ~f:(fun nm ->
    if String.is_empty nm
       || not (String.for_all nm ~f:(fun c -> Char.is_alphanum c || String.mem "_." c))
    then raise_s [%message "Sdc.clock: names are [A-Za-z0-9_.]+" nm]);
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

(** A {!Vernier} instance [name] with [pins] pins, clocked at [period_ns],
    whose host never loads a [settle_cycles] below the given one.

    Its lines are asynchronous delay paths from the launch flops
    ([<name>_p<i>_{r,f}_launch]) through the XOR ([<name>_p<i>_xor]) to the
    pin, and their tap selects change only while no edge is in flight, so
    none of these paths is a single-cycle path. The control relies instead
    on two bounds, which become path delays to internal pins (STA makes
    those path endpoints and, unless [-probe], breaks paths through them):
    - {e select}, per line: [set_max_delay period_ns] to the select pin of
      every mux of the line (coarse and fine trees, coarse and fine bypass):
      a select is loaded, and a mode applied, one clock edge before the
      launch it is for. The arrival includes the source clock latency and
      the required time no capture latency, which errs on the safe side by
      the launch flop's latency and clk->Q.
    - {e drain}, per pin: [set_max_delay (settle_cycles * period_ns)] from
      both launch flops to the output mux's input from the XOR
      ([<name>_p<i>_out]): every chain, tree and bypass node and the XOR lie
      on a path there, so once an edge has arrived every select may change
      and the output mux may switch. The synchronised loopback, which the
      [mismatch] check compares [settle_cycles + 2] edges after a launch,
      relies on it too.
    The rest is cut: false paths from the launch flops to every register
    (the loopback synchroniser, a capture block on [line_out]) and every
    output port (the pin itself, [loopback], [line_out]). The synchronous
    bypass path (level flop -> output mux -> pin) stays timed. Select paths
    past the select pins are cut only by the break at those pins.

    The constraints appear in both stages. The select nets into the muxes
    are boundary nets, protected and never repaired: their fanout is what
    synthesis left ({!boundary_fanouts}); the drain paths are all
    [dont_touch]. At PnR both are checks. OpenSTA warns (1551) that each
    internal pin "is not a valid endpoint" and honours it. The cells are
    protected like every [dont_touch] instance. *)
let vernier t ~name ~pins ~period_ns ~settle_cycles =
  if not (Float.is_finite period_ns && Float.(period_ns > 0.))
  then raise_s [%message "Sdc.vernier: bad period" (period_ns : float)];
  if settle_cycles < 1 || settle_cycles >= 1 lsl Vernier.settle_bits
  then raise_s [%message "Sdc.vernier: settle_cycles out of range" (settle_cycles : int)];
  let driven_by (e : Endpoint.t) =
    match t.nets.(pin_net t e).driver with
    | Cell d -> Some d.instance
    | Register _ | Constant _ | Input | Other -> None
  in
  (* a mux's select: its one input not driven by another cell of the line
     (but from synthesised logic, or its select buffers [<line>_sel_...]) *)
  let select ~line instance =
    let from_line e =
      match driven_by e with
      | Some d -> not (String.is_prefix d ~prefix:(line ^ "_sel_"))
      | None -> false
    in
    match List.filter (input_pins t instance) ~f:(fun e -> not (from_line e)) with
    | [ e ] -> e
    | l -> raise_s [%message "Sdc.vernier: expected one select input" (instance : string) (l : Endpoint.t list)]
  in
  let selects prefix =
    let tree =
      List.filter t.instances ~f:(fun (i : Instance.t) ->
        List.exists [ "_cm"; "_fm" ] ~f:(fun s -> String.is_prefix i.name ~prefix:(prefix ^ s)))
    in
    if List.is_empty tree then raise_s [%message "Sdc.vernier: no tap muxes" (prefix : string)];
    List.map tree ~f:(fun i -> select ~line:prefix i.name)
    @ List.map [ prefix ^ "_cbyp"; prefix ^ "_fbyp" ] ~f:(select ~line:prefix)
  in
  List.fold (List.init pins ~f:Fn.id) ~init:t ~f:(fun t i ->
    let p = Printf.sprintf "%s_p%d" name i in
    let launch = [ p ^ "_r_launch"; p ^ "_f_launch" ] in
    List.iter launch ~f:(fun l ->
      if Option.is_none (find_instance t l) then raise_s [%message "Sdc: no protected instance" ~instance:(l : string)]);
    let xor = p ^ "_xor" in
    let from_xor =
      match
        List.filter (input_pins t (p ^ "_out")) ~f:(fun e ->
          Option.equal String.equal (driven_by e) (Some xor))
      with
      | [ e ] -> e
      | l -> raise_s [%message "Sdc.vernier: expected one output mux input from the XOR" (p : string) (l : Endpoint.t list)]
    in
    { t with
      false_paths_from =
        t.false_paths_from
        @ [ { False_path_from.from = launch; to_ = All_registers }; { from = launch; to_ = All_outputs } ]
    ; max_delays =
        t.max_delays
        @ [ { Max_delay.delay_ns = period_ns; from = []; to_ = selects (p ^ "_r") }
          ; { delay_ns = period_ns; from = []; to_ = selects (p ^ "_f") }
          ; { delay_ns = Float.of_int settle_cycles *. period_ns; from = launch; to_ = [ from_xor ] }
          ]
    })
;;

let max_delays t = t.max_delays

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

(* An anchored Tcl ARE matching exactly [names], factored into a character
   trie with non-capturing groups: islands have thousands of structured
   names (ring_inv12, vernier_p0_r_cm1_7, ...), and LibreLane passes the
   regex to OpenROAD in an environment variable (Linux: at most 128 KiB). *)
let dont_touch_rx names =
  let rec trie names =
    let ends = List.exists names ~f:String.is_empty in
    let branches =
      List.filter names ~f:(fun s -> not (String.is_empty s))
      |> List.group ~break:(fun a b -> not (Char.equal a.[0] b.[0]))
      |> List.map ~f:(fun g ->
        rx_of_name (String.prefix (List.hd_exn g) 1)
        ^ trie (List.map g ~f:(fun s -> String.drop_prefix s 1)))
    in
    match branches, ends with
    | [], _ -> ""
    | [ b ], false -> b
    | bs, false -> "(?:" ^ String.concat ~sep:"|" bs ^ ")"
    | bs, true -> "(?:" ^ String.concat ~sep:"|" bs ^ ")?"
  in
  match List.dedup_and_sort names ~compare:String.compare with
  | [] -> "$^"
  | names -> "^" ^ trie names ^ "$"
;;

let rsz_dont_touch_rx t = dont_touch_rx (instance_names t @ net_names t)

(* Names go into SDC braced (a list of them, space separated); refuse what
   would break the braces. *)
let tcl_names names =
  List.iter names ~f:(fun nm ->
    if String.exists nm ~f:(fun c -> Char.is_whitespace c || String.mem "{}\\" c)
    then raise_s [%message "Sdc: name not usable in SDC" nm]);
  "{" ^ String.concat ~sep:" " names ^ "}"
;;

let tcl_name nm = tcl_names [ nm ]

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
  List.iter t.false_paths_from ~f:(fun f ->
    line
      "set_false_path -from [get_cells %s] -to [%s]"
      (tcl_names (List.map f.from ~f:(fun i -> t.prefix ^ i)))
      (match f.to_ with
       | All_registers -> "all_registers"
       | All_outputs -> "all_outputs"));
  List.iter t.max_delays ~f:(fun d ->
    line
      "set_max_delay %.4f%s -to [get_pins %s]"
      d.delay_ns
      (match d.from with
       | [] -> ""
       | f -> Printf.sprintf " -from [get_cells %s]" (tcl_names (List.map f ~f:(fun i -> t.prefix ^ i))))
      (tcl_names (List.map d.to_ ~f:(fun e -> t.prefix ^ Endpoint.to_string e))));
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
     if not (List.is_empty t.clocks)
     then
       line
         "set_propagated_clock [get_clocks {%s}]"
         (String.concat ~sep:" " (List.map t.clocks ~f:(fun c -> c.name)));
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
let lib_cell t (library : Timing_library.t) (i : Netlist.Instance.t) =
  match Timing_library.find library i.cell with
  | Some c -> Some c
  | None ->
    Option.bind
      (if String.is_prefix i.name ~prefix:t.prefix
       then find_instance t (String.drop_prefix i.name (String.length t.prefix))
       else None)
      ~f:(fun (e : Instance.t) -> Timing_library.find library e.cell)
;;

(* Drivers and loads of every netlist net, by pin direction. Pins the library
   does not list on a cell it knows (power) are ignored; bidirectional pins
   and every pin of a cell it does not know count as loads. *)
let drivers_and_loads t library (nl : Netlist.t) =
  let drivers = Array.create ~len:(Netlist.num_nets nl) [] in
  let loads = Array.create ~len:(Netlist.num_nets nl) [] in
  List.iter (Netlist.instances nl) ~f:(fun i ->
    let cell = lib_cell t library i in
    List.iter i.pins ~f:(fun (pin, c) ->
      match c with
      | Open -> ()
      | Net n ->
        (match cell with
         | None -> loads.(n) <- (i, pin) :: loads.(n)
         | Some c ->
           (match Cell_library.Cell.pin c.logic pin with
            | Some { direction = Output; _ } -> drivers.(n) <- (i, pin) :: drivers.(n)
            | Some { direction = Input | Inout; _ } -> loads.(n) <- (i, pin) :: loads.(n)
            | Some { direction = Internal; _ } | None -> ()))));
  drivers, loads
;;

let pin_conn (i : Netlist.Instance.t) pin =
  match List.Assoc.find i.pins pin ~equal:String.equal with
  | Some (Net n) -> Some n
  | Some Open | None -> None
;;

let single_var : Bool_expr.t -> (string * bool) option = function
  | Var v -> Some (v, true)
  | Not (Var v) -> Some (v, false)
  | _ -> None
;;

(* Cells that only load a net (antenna diodes): Liberty cells with only inputs. *)
let passive_of ?passive (library : Timing_library.t) =
  match passive with
  | Some f -> f
  | None ->
    fun cell ->
      (match Timing_library.find library cell with
       | Some c ->
         List.for_all c.logic.pins ~f:(fun pin ->
           match pin.direction with
           | Output | Inout -> false
           | Input | Internal -> true)
       | None -> false)
;;

(** [check_structure t ~library netlist] lists every way [netlist] (yosys or OpenROAD
    output) departs from the islands; [[]] if they survived:
    - every protected instance exists with the same cell type;
    - every protected net is a single netlist net with one driver, carries one
      of its circuit names (so the dont-touch regex matched it) or a name in
      [dont_touch] (the resolved names), and, if renamed, one it had in
      [reference] (the synthesised netlist the names were resolved from:
      which net it is, beyond being protected), still drives its circuit
      outputs, and, unless it also feeds synthesised logic, has no loads but
      its island sinks and [passive] cells (default: Liberty cells with only
      inputs, e.g. antenna diodes). Loads from synthesised logic are not
      checked one by one, nor loads on boundary nets (synthesis may share
      them), nor, with a [prefix], loads on the island's inputs and outputs
      (the parent's logic);
    - every kept register is a plain D flop with the right clock edge, its D on
      the expected net, its clock from the circuit's clock and, if the
      register had no reset, its clear/preset tied inactive;
    - inputs entering the island may be rebuffered but must come from the net
      that still carries their circuit name ([boundary] maps circuit names to
      netlist names, e.g. after flattening). Circuit inputs whose names are
      all gone are reported as unverifiable. Constants may be literals or tie
      cells of the right value. *)
let check_structure ?passive ?(boundary = []) ?(dont_touch = []) ?reference t ~(library : Timing_library.t) (nl : Netlist.t) =
  let errors = Queue.create () in
  let dont_touch = Set.of_list (module String) dont_touch in
  let err fmt = Printf.ksprintf (Queue.enqueue errors) fmt in
  let p nm = t.prefix ^ nm in
  let passive = passive_of ?passive library in
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
      (match pin_conn i e.pin with
       | Some n -> Some n
       | None ->
         err "%s is not connected" (p (Endpoint.to_string e));
         None)
  in
  let is_protected_instance (i : Netlist.Instance.t) =
    String.is_prefix i.name ~prefix:t.prefix
    && Hashtbl.mem found (String.drop_prefix i.name (String.length t.prefix))
  in
  (* trace back through unprotected buffers (of [nl], or with [drivers] of
     another netlist) *)
  let rec root_in drivers n depth =
    if depth > 64 || n = Netlist.const0 || n = Netlist.const1
    then n
    else (
      match drivers.(n) with
      | [ (i, _) ] when not (is_protected_instance i) ->
        (match Timing_library.find library i.cell with
         | Some c when Cell_library.Cell.is_buffer c.logic ->
           let input = (List.hd_exn (Cell_library.Cell.inputs c.logic)).name in
           (match pin_conn i input with
            | Some m -> root_in drivers m (depth + 1)
            | None -> n)
         | _ -> n)
      | _ -> n)
  in
  let root n depth = root_in drivers n depth in
  let reference_drivers = Option.map reference ~f:(fun r -> fst (drivers_and_loads t library r)) in
  let tie_value n =
    if n = Netlist.const0
    then Some false
    else if n = Netlist.const1
    then Some true
    else (
      match drivers.(n) with
      | [ (i, pin) ] ->
        (match Timing_library.find library i.cell with
         | Some c when Cell_library.Kind.equal c.logic.kind Constant ->
           (match Cell_library.Cell.pin c.logic pin with
            | Some { function_ = Some (Const b); _ } -> Some b
            | _ -> None)
         | _ -> None)
      | _ -> None)
  in
  (* The netlist net a circuit net corresponds to, from its endpoints or, for
     nets entering the island, a surviving name ([boundary] maps circuit names
     to netlist names where flattening renamed them). *)
  let by_name (net : Net.t) =
    List.find_map net.names ~f:(fun nm ->
      match List.Assoc.find boundary nm ~equal:String.equal with
      | Some x -> Netlist.find_net nl x
      | None -> Netlist.find_net nl (p nm))
  in
  let resolve (net : Net.t) =
    let ends =
      (match net.driver with
       | Cell e -> [ e ]
       | _ -> [])
      @ net.sinks
    in
    match List.find_map ends ~f:netlist_net with
    | Some n -> Some n
    | None ->
      (match net.driver with
       | Register r -> Netlist.find_net nl (p r)
       | _ -> by_name net)
  in
  let reference_names (net : Net.t) =
    Option.bind reference ~f:(fun r ->
      List.find_map
        ((match net.driver with
          | Cell e -> [ e ]
          | _ -> [])
         @ net.sinks)
        ~f:(fun (e : Endpoint.t) ->
          Option.bind (Netlist.find_instance r (p e.instance)) ~f:(fun i -> pin_conn i e.pin))
      |> Option.map ~f:(Netlist.net_names r))
  in
  let label (net : Net.t) =
    match net.names with
    | nm :: _ -> p nm
    | [] -> "<unnamed>"
  in
  (* nets entering the island: [n] must come from the same source as [net] *)
  let check_source (net : Net.t) what n =
    match net.driver with
    | Constant b ->
      (match tie_value (root n 0) with
       | Some v when Bool.equal v b -> ()
       | _ -> err "%s should be constant %d, is on %s" what (Bool.to_int b) (net_str (root n 0)))
    | _ ->
      (match by_name net, net.driver with
       | Some m, _ ->
         if root m 0 <> root n 0
         then err "%s is driven from %s, not from %s" what (net_str (root n 0)) (net_str m)
       | None, Input when not (List.is_empty net.names) ->
         err
           "%s: cannot verify its source, none of %s survives (pass ~boundary)"
           what
           (String.concat ~sep:"," (List.map net.names ~f:p))
       | None, _ ->
         (* a boundary net: synthesis may rename it (yosys prefers generated
            whole-wire names to bits of a named bus); it is checked as one
            protected net *)
         ())
  in
  (* isolating buffers: present, between their two nets (the input may be
     rebuffered and the buffer resized: it is the resizer's) *)
  List.iter t.isolators ~f:(fun (name, input, output) ->
    match Netlist.find_instance nl (p name) with
    | None -> err "missing isolating buffer %s" (p name)
    | Some i ->
      (match Timing_library.find library i.cell with
       | Some c when Cell_library.Cell.is_buffer c.logic ->
         let pin f = pin_conn i (List.hd_exn (f c.logic) : Cell_library.Pin.t).name in
         (match pin Cell_library.Cell.outputs, resolve t.nets.(output) with
          | Some o, Some expected when o <> expected ->
            err "isolating buffer %s drives %s, not %s" (p name) (net_str o) (net_str expected)
          | None, _ -> err "isolating buffer %s: output not connected" (p name)
          | _ -> ());
         let what = "isolating buffer " ^ p name in
         (match pin Cell_library.Cell.inputs with
          | None -> err "%s: input not connected" what
          | Some n ->
            check_source t.nets.(input) what n;
            (* a source whose circuit names did not survive synthesis:
               against the reference *)
            if Option.is_none (by_name t.nets.(input))
            then (
              match reference with
              | None ->
                (match t.nets.(input).driver with
                 | Other -> err "%s: cannot verify its source (pass ~reference)" what
                 | _ -> ())
              | Some r ->
                (match
                   Option.bind (Netlist.find_instance r (p name)) ~f:(fun ri ->
                     pin_conn ri (List.hd_exn (Cell_library.Cell.inputs c.logic) : Cell_library.Pin.t).name)
                 with
                 | None -> err "%s: not in the reference" what
                 | Some rn ->
                   let want =
                     Netlist.net_names r (root_in (Option.value_exn reference_drivers) rn 0)
                   in
                   if not
                        (List.exists (Netlist.net_names nl (root n 0)) ~f:(fun nm ->
                           List.mem want nm ~equal:String.equal))
                   then
                     err
                       "%s is fed from %s, from %s in the reference"
                       what
                       (net_str (root n 0))
                       (String.concat ~sep:"," want))))
       | _ -> err "isolating buffer %s is %s, not a buffer" (p name) i.cell));
  (* flops synthesised from kept registers, by their D net *)
  let reg_flops = Hashtbl.create (module Int) in
  List.iter t.registers ~f:(fun r ->
    let what = "kept register " ^ p r.name in
    match Netlist.find_net nl (p r.name) with
    | None -> err "%s: no net of that name" what
    | Some q ->
      (match drivers.(q) with
       | [ (i, _) ] ->
         (match Timing_library.find library i.cell with
          | Some { seq = Some ({ kind = Ff { clocked_on; next_state }; _ } as seq); _ } ->
            (match single_var next_state, single_var clocked_on with
             | Some (dpin, true), Some (cpin, rising) ->
               (match pin_conn i dpin with
                | None -> err "%s: data pin %s/%s is not connected" what i.name dpin
                | Some dn ->
                  Hashtbl.add_multi reg_flops ~key:r.d ~data:(i.name, dpin);
                  (match resolve t.nets.(r.d) with
                   | Some expected when Net.is_protected t.nets.(r.d) ->
                     if dn <> expected
                     then err "%s: D is on %s, expected %s" what (net_str dn) (net_str expected)
                   | _ -> check_source t.nets.(r.d) (what ^ " D") dn));
               if Bool.( <> ) rising r.rising then err "%s: flop %s has the wrong clock edge" what i.name;
               (match pin_conn i cpin with
                | None -> err "%s: clock pin %s/%s is not connected" what i.name cpin
                | Some cn -> check_source t.nets.(r.clock) (what ^ " clock") cn);
               if not r.has_reset
               then
                 (* asynchronous clear/preset must be tied inactive *)
                 List.iter (List.filter_opt [ seq.clear; seq.preset ]) ~f:(fun e ->
                   let env v =
                     match Option.bind (pin_conn i v) ~f:(fun n -> tie_value (root n 0)) with
                     | Some b -> b
                     | None ->
                       err "%s: %s/%s is not tied" what i.name v;
                       false
                   in
                   if Bool_expr.eval e ~env then err "%s: flop %s is held in reset" what i.name)
             | _ -> err "%s: %s (%s) is not a plain D flop" what i.name i.cell)
          | _ -> err "%s is driven by %s (%s), not a flop" what i.name i.cell)
       | ds -> err "%s: net has %d drivers" what (List.length ds)));
  Array.iteri t.nets ~f:(fun idx net ->
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
      match List.dedup_and_sort (List.map mapped ~f:snd) ~compare:Int.compare with
      | _ :: _ :: _ ->
        err
          "net %s is split: %s"
          (label net)
          (String.concat
             ~sep:"; "
             (List.map mapped ~f:(fun (e, n) -> Endpoint.to_string e ^ " on " ^ net_str n)))
      | ns ->
        let n =
          match ns, net.driver with
          | [ n ], _ -> Some n
          | _, Register r -> Netlist.find_net nl (p r)
          | _ -> None
        in
        Option.iter n ~f:(fun n ->
          let have = Netlist.net_names nl n in
          let by_circuit_name =
            List.exists net.names ~f:(fun nm -> List.mem have (p nm) ~equal:String.equal)
          in
          (* renamed: it must be the net it was in the reference *)
          (match reference_names net with
           | Some names
             when (not by_circuit_name)
                  && not (List.exists have ~f:(List.mem names ~equal:String.equal)) ->
             err
               "net %s is %s in the netlist, %s in the reference"
               (label net)
               (net_str n)
               (String.concat ~sep:"," names)
           | _ -> ());
          if (not (List.is_empty net.names))
             && (not by_circuit_name)
             && not (List.exists have ~f:(Set.mem dont_touch))
          then
            err
              "net %s is named %s in the netlist: the dont-touch names do not match it"
              (label net)
              (net_str n);
          (match drivers.(n), net.driver with
           | [ _ ], _ -> ()
           | [], (Constant _ | Input) -> () (* an input port *)
           | ds, _ ->
             err
               "net %s has %d drivers: %s"
               (label net)
               (List.length ds)
               (String.concat ~sep:", " (List.map ds ~f:(fun (i, pin) -> i.name ^ "/" ^ pin))));
          (* a fork fed from outside keeps its source *)
          (match net.driver with
           | Input | Other | Constant _ -> check_source net ("net " ^ label net) n
           | Cell _ | Register _ -> ());
          (* circuit outputs (top level only: in a flattened hierarchy they are
             the parent's logic) *)
          let port_sinks = (not (String.is_empty t.prefix)) && not (List.is_empty net.ports) in
          if String.is_empty t.prefix
          then
            List.iter net.ports ~f:(fun port ->
              match Netlist.find_net nl port with
              | Some m when root m 0 = n -> ()
              | _ -> err "net %s no longer drives output %s" (label net) port);
          (* boundary nets are synthesised logic, which synthesis may share
             with other logic *)
          if not (net.logic_sinks || port_sinks || net.boundary)
          then (
            let expected =
              List.filter_map net.sinks ~f:(fun e ->
                Option.map (Hashtbl.find found e.instance) ~f:(fun i -> i.name, e.pin))
              @ Hashtbl.find_multi reg_flops idx
            in
            List.iter loads.(n) ~f:(fun (i, pin) ->
              let port_buffer =
                (* LibreLane's insbuf between a net and an output port *)
                String.is_empty t.prefix
                && (match Timing_library.find library i.cell with
                    | Some c when Cell_library.Cell.is_buffer c.logic ->
                      List.exists net.ports ~f:(fun port ->
                        match Netlist.find_net nl port with
                        | Some m -> List.exists drivers.(m) ~f:(fun (d, _) -> phys_equal d i)
                        | None -> false)
                    | _ -> false)
              in
              if not
                   (List.mem expected (i.name, pin) ~equal:[%equal: string * string]
                    || passive i.cell
                    || port_buffer)
              then err "net %s has an extra load %s/%s (%s)" (label net) i.name pin i.cell))))
    else
      (* inputs from outside the island: sinks may be rebuffered, but must
         keep a common source; constants may become tie cells *)
      List.iter mapped ~f:(fun (e, n) -> check_source net (p (Endpoint.to_string e)) n));
  Queue.to_list errors
;;

(** Exact names to protect, from a synthesised netlist: protected
    instances, the netlist names of protected nets, and the flops
    synthesised from kept registers. An error if any is missing.

    Give them to LibreLane as [RSZ_DONT_TOUCH_RX] ({!dont_touch_rx}), not as
    [RSZ_DONT_TOUCH_LIST]: LibreLane 3.1 passes list elements Tcl-escaped
    ([uio_in\[2]]), and OpenROAD's [set_dont_touch] then does not find
    bus-bit names (STA-0101) and leaves them unprotected. *)
let dont_touch_list t ~(library : Timing_library.t) (nl : Netlist.t) =
  let p nm = t.prefix ^ nm in
  let drivers, _ = drivers_and_loads t library nl in
  let missing = Queue.create () in
  let insts =
    List.filter_map t.instances ~f:(fun i ->
      match Netlist.find_instance nl (p i.name) with
      | Some x -> Some x.name
      | None ->
        Queue.enqueue missing ("instance " ^ p i.name);
        None)
  in
  let net_of_endpoint (e : Endpoint.t) =
    Option.bind (Netlist.find_instance nl (p e.instance)) ~f:(fun i -> pin_conn i e.pin)
  in
  let nets =
    List.concat_map (protected_nets t) ~f:(fun net ->
      let n =
        match net.driver, net.sinks with
        | Cell e, _ -> net_of_endpoint e
        | _, e :: _ -> net_of_endpoint e
        | Register r, [] -> Netlist.find_net nl (p r)
        | _ -> List.find_map net.names ~f:(fun nm -> Netlist.find_net nl (p nm))
      in
      match n with
      | None ->
        Queue.enqueue
          missing
          ("net " ^ Option.value_map (List.hd net.names) ~default:"<unnamed>" ~f:p);
        []
      | Some n ->
        let flop =
          match net.driver, drivers.(n) with
          | Register _, [ ((i : Netlist.Instance.t), _) ] -> [ i.name ]
          | Register r, _ ->
            Queue.enqueue missing ("flop of kept register " ^ p r);
            []
          | _ -> []
        in
        Netlist.net_names nl n @ flop)
  in
  if Queue.is_empty missing
  then Ok (List.dedup_and_sort ~compare:String.compare (insts @ nets))
  else
    Or_error.error_s
      [%message "Sdc.dont_touch_list: not in the netlist" ~_:(Queue.to_list missing : string list)]
;;

(** Protected nets of [netlist] with more than [max_fanout] loads (passive
    cells, e.g. antenna diodes, not counted), with their load counts, worst
    first. OpenROAD repairs no protected net (and could not buffer a net
    with a dont-touch load: RSZ-3006), so their fanout, slew and capacitance
    stay as the island and synthesis left them: build fanout from cells
    ({!Cell.fanout}) and isolate shared controls ({!Cell.isolate}). Pass the
    smaller of the PDK's MAX_FANOUT_CONSTRAINT and the Liberty
    [default_max_fanout] ({!Cell.max_fanout}). *)
let fanout_violations ?passive t ~(library : Timing_library.t) ~max_fanout (nl : Netlist.t) =
  let passive = passive_of ?passive library in
  let _, loads = drivers_and_loads t library nl in
  List.filter_map (Array.to_list t.nets) ~f:(fun (net : Net.t) ->
    if not (Net.is_protected net)
    then None
    else
      Option.bind
        (match
           List.find_map
             ((match net.driver with
               | Cell e -> [ e ]
               | _ -> [])
              @ net.sinks)
             ~f:(fun e ->
               Option.bind (Netlist.find_instance nl (t.prefix ^ e.instance)) ~f:(fun i ->
                 pin_conn i e.pin))
         with
         | Some n -> Some n
         | None ->
           (match net.driver with
            | Register r -> Netlist.find_net nl (t.prefix ^ r)
            | _ -> None))
        ~f:(fun n ->
          let fanout =
            List.count loads.(n) ~f:(fun ((i : Netlist.Instance.t), _) -> not (passive i.cell))
          in
          Option.some_if
            (fanout > max_fanout)
            (String.concat ~sep:"," (Netlist.net_names nl n), fanout)))
  |> List.dedup_and_sort ~compare:[%compare: string * int]
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a)
;;

(** {!check_structure}, and with [max_fanout] also every protected net above
    it ({!fanout_violations}). *)
let check ?passive ?boundary ?dont_touch ?reference ?max_fanout t ~library nl =
  check_structure ?passive ?boundary ?dont_touch ?reference t ~library nl
  @ (match max_fanout with
     | None -> []
     | Some max_fanout ->
       List.map (fanout_violations ?passive t ~library ~max_fanout nl) ~f:(fun (n, f) ->
         Printf.sprintf
           "net %s has %d loads (max %d): OpenROAD repairs no protected net"
           n
           f
           max_fanout))
;;

(* --- Placement --- *)

(** Standard-cell rows: the origin of row 0, the row height and the site
    width (µm), and whether row 0 is flipped (FS); rows alternate. *)
type rows =
  { x0 : float
  ; y0 : float
  ; row_height : float
  ; site_width : float
  ; first_flipped : bool
  }

(** [ring_placement t ~name ~library ~rows ~site ~row ~columns] places ring
    [name] and the protected cells its nodes drive (sampler first stages,
    capture data buffers, counter clock inverters) as a compact, legal
    block of standard cells, for a placement that is the same in every run:
    the ring's period depends on its node loads, which an unconstrained
    placer spreads out (doubling the period in the gate). Stage [k] and the
    protected loads of its output form a group; groups snake down and up
    [columns] columns (up the first, down the next...) so that consecutive
    stages are neighbours, and with two columns the last and first too;
    the counters and the enable tree go in the rows below
    them. The block's lower-left corner is site [site] of row [row]. Consecutive
    groups are [row_pitch] rows (default 1) and the columns [column_gap]
    sites (default 0) apart. The coordinates are deterministic; the wire
    load they give is a measured result for a geometry, not a guarantee: in
    the gate (TT IHP sg13cmos5l, 2 columns) a compact block left the nodes
    with a fifth of the Liberty "1k" wire load the ring's timing (its
    counters' L and E, the Gray counter's settling within a period) was
    characterised with, an 11-row pitch and 50-site gap with 0.93 of it.
    Revalidate after changing the geometry. Returns
    [(instance, x, y, orientation)] for LibreLane's [MACRO_PLACEMENT_CFG]
    (fixed before global placement, which then pulls the connected logic
    towards the block). Cell widths are their Liberty area over the row
    height. *)
let ring_placement ?(row_pitch = 1) ?(column_gap = 0) t ~name ~(library : Cell_library.t) ~rows ~site ~row ~columns =
  if columns < 1 || row_pitch < 1 || column_gap < 0 || site < 0 || row < 0
  then
    raise_s
      [%message "Sdc.ring_placement: bad geometry" (columns : int) (row_pitch : int) (column_gap : int) (site : int) (row : int)];
  let width i =
    let c = Cell_library.find_exn library i.Instance.cell in
    Float.to_int (Float.round_up (c.area /. rows.row_height /. rows.site_width -. 1e-6))
  in
  let inst nm =
    match find_instance t nm with
    | Some i -> i
    | None -> raise_s [%message "Sdc.ring_placement: no instance" nm]
  in
  let is_stage nm =
    String.equal nm (name ^ "_nand")
    || List.exists [ "_inv"; "_mux" ] ~f:(fun s -> String.is_prefix nm ~prefix:(name ^ s))
  in
  let taken = Hash_set.create (module String) in
  (* walk the loop from the NAND *)
  let rec walk acc (i : Instance.t) =
    Hash_set.add taken i.name;
    let out = output_pin t i.name in
    let n = pin_net t out in
    let next = List.find t.nets.(n).sinks ~f:(fun e -> is_stage e.instance && not (Hash_set.mem taken e.instance)) in
    let acc = (i, n) :: acc in
    match next with
    | Some e -> walk acc (inst e.instance)
    | None -> List.rev acc
  in
  let stages = walk [] (inst (name ^ "_nand")) in
  let groups =
    List.map stages ~f:(fun (i, n) ->
      let loads =
        List.filter_map t.nets.(n).sinks ~f:(fun e ->
          if is_stage e.instance || Hash_set.mem taken e.instance
          then None
          else (
            Hash_set.add taken e.instance;
            Some (inst e.instance)))
        |> List.dedup_and_sort ~compare:(fun (a : Instance.t) b -> String.compare a.name b.name)
      in
      i :: loads)
  in
  let extras =
    List.filter t.instances ~f:(fun i ->
      String.is_prefix i.name ~prefix:(name ^ "_") && not (Hash_set.mem taken i.name))
  in
  let group_width g = List.sum (module Int) g ~f:width + 1 in
  let per_column = (List.length groups + columns - 1) / columns in
  let column_width = List.fold groups ~init:0 ~f:(fun m g -> Int.max m (group_width g)) in
  let place = Queue.create () in
  let put (i : Instance.t) ~s ~r =
    let r = row + r in
    let flipped = Bool.( <> ) (r % 2 = 1) rows.first_flipped in
    Queue.enqueue
      place
      ( t.prefix ^ i.name
      , rows.x0 +. (Float.of_int (site + s) *. rows.site_width)
      , rows.y0 +. (Float.of_int r *. rows.row_height)
      , if flipped then "FS" else "N" )
  in
  (* the extras, in rows downwards from the block's first: their row count
     sets where the block starts *)
  let total = (columns * column_width) + ((columns - 1) * column_gap) in
  let extras, extra_rows =
    List.fold extras ~init:([], 0, 0) ~f:(fun (acc, s, j) i ->
      let s, j = if s + width i > total && s > 0 then 0, j + 1 else s, j in
      (i, s, j) :: acc, s + width i, j)
    |> fun (acc, _, j) -> List.rev acc, if List.is_empty acc then 0 else j + 1
  in
  List.iter extras ~f:(fun (i, s, j) -> put i ~s ~r:(extra_rows - 1 - j));
  List.iteri groups ~f:(fun k g ->
    let col = k / per_column in
    let r =
      extra_rows + (row_pitch * if col % 2 = 0 then k % per_column else per_column - 1 - (k % per_column))
    in
    ignore
      (List.fold g ~init:(col * (column_width + column_gap)) ~f:(fun s i ->
         put i ~s ~r;
         s + width i)
       : int));
  Queue.to_list place
  |> List.sort ~compare:(fun (a, _, _, _) (b, _, _, _) -> String.compare a b)
;;
