(** Timing view of a Liberty library: NLDM delay and slew tables per timing
    arc, input pin capacitances, flop/latch state machines, setup/hold and
    recovery/removal constraints, minimum pulse widths and wire-load models.

    All quantities are normalised on load to nanoseconds and picofarads,
    whatever the library's [time_unit] and [capacitive_load_unit].

    Tables are normalised so that [index_1] is always the {e input} side
    ([input_net_transition] for delays and slews, [constrained_pin_transition]
    for constraints) and [index_2] the {e output/related} side
    ([total_output_net_capacitance], [related_pin_transition]); templates
    declaring the other order are transposed. *)

open! Base

module Table = struct
  type t =
    { index_1 : float array
    ; index_2 : float array
    ; values : float array array (** [values.(i).(j)] at [index_1.(i)], [index_2.(j)] *)
    }
  [@@deriving sexp_of]

  (* The segment of [xs] used for [x], and the position of [x] along it
     (outside [0, 1] when extrapolating, as STA tools do). *)
  let segment xs x =
    let n = Array.length xs in
    if n = 1
    then 0, 0, 0.
    else (
      let j = ref 0 in
      while !j < n - 2 && Float.( > ) x xs.(!j + 1) do
        Int.incr j
      done;
      let x0 = xs.(!j)
      and x1 = xs.(!j + 1) in
      !j, !j + 1, (x -. x0) /. (x1 -. x0))
  ;;

  (** Bilinear interpolation, linear extrapolation outside the table. *)
  let lookup t x1 x2 =
    let i0, i1, fi = segment t.index_1 x1 in
    let j0, j1, fj = segment t.index_2 x2 in
    let v i j = t.values.(i).(j) in
    let a = v i0 j0 +. (fj *. (v i0 j1 -. v i0 j0)) in
    let b = v i1 j0 +. (fj *. (v i1 j1 -. v i1 j0)) in
    a +. (fi *. (b -. a))
  ;;
end

module Arc = struct
  type t =
    { related_pin : string
    ; timing_type : string (** [combinational], [rising_edge], [clear], [setup_rising], ... *)
    ; timing_sense : string option
    ; when_ : Bool_expr.t option
    ; cell_rise : Table.t option
    ; cell_fall : Table.t option
    ; rise_transition : Table.t option
    ; fall_transition : Table.t option
    ; rise_constraint : Table.t option
    ; fall_constraint : Table.t option
    }
  [@@deriving sexp_of]

  (** Setup, hold, recovery and removal arcs (a constrained input against a
      related clock edge). *)
  let is_constraint t =
    List.exists [ "setup_"; "hold_"; "recovery_"; "removal_" ] ~f:(fun prefix ->
      String.is_prefix t.timing_type ~prefix)
  ;;
end

(** Minimum pulse widths of an input pin, ns: [high] is the shortest time the
    pin may stay at 1, [low] at 0. From [min_pulse_width] timing groups
    ([rise_constraint] high, [fall_constraint] low, indexed by the pin's own
    transition; look up with the slew on both axes, since templates put it on
    either) or the constant [min_pulse_width_high]/[min_pulse_width_low] pin
    attributes, which take precedence (as in OpenSTA). [when] conditions are
    ignored. *)
module Pulse_width = struct
  type t =
    { high : Table.t option
    ; low : Table.t option
    }
  [@@deriving sexp_of]

  let lookup tbl ~slew = Table.lookup tbl slew slew
end

(** A [wire_load] model: wire capacitance as a function of a net's fanout. *)
module Wire_load = struct
  type t =
    { name : string
    ; capacitance : float (** pF per unit length *)
    ; slope : float (** length per fanout beyond the table *)
    ; fanout_length : (int * float) list (** ascending fanout *)
    }
  [@@deriving sexp_of]

  (** Estimated wire length, as OpenSTA computes it: linear between table
      points, extrapolated with [slope] outside them (not below 0). *)
  let length t ~fanout =
    let f = Float.of_int fanout in
    match t.fanout_length with
    | [] -> 0.
    | (f0, l0) :: _ as pts ->
      let fmax, lmax = List.last_exn pts in
      if fanout <= f0
      then Float.max 0. (l0 -. ((Float.of_int f0 -. f) *. t.slope))
      else if fanout >= fmax
      then lmax +. ((f -. Float.of_int fmax) *. t.slope)
      else (
        let rec go = function
          | (fa, la) :: ((fb, lb) :: _ as rest) ->
            if fanout <= fb
            then la +. ((lb -. la) *. (f -. Float.of_int fa) /. Float.of_int (fb - fa))
            else go rest
          | _ -> lmax
        in
        go pts)
  ;;

  (** Wire capacitance, pF. *)
  let cap_pf t ~fanout = length t ~fanout *. t.capacitance
end

module Seq = struct
  type kind =
    | Ff of
        { clocked_on : Bool_expr.t
        ; next_state : Bool_expr.t
        }
    | Latch of
        { enable : Bool_expr.t
        ; data_in : Bool_expr.t
        }
  [@@deriving sexp_of]

  type t =
    { state : string (** e.g. [IQ] *)
    ; state_n : string (** e.g. [IQN] *)
    ; kind : kind
    ; clear : Bool_expr.t option
    ; preset : Bool_expr.t option
    ; clear_preset_var1 : string option (** [L], [H], [N], [T] or [X]; [X] if absent *)
    }
  [@@deriving sexp_of]
end

module Cell = struct
  type t =
    { name : string
    ; logic : Cell_library.Cell.t
    ; pin_cap : (string * float) list (** input pins, pF *)
    ; arcs : (string * Arc.t list) list (** output pin -> delay arcs *)
    ; constraints : (string * Arc.t list) list
    (** input pin -> setup/hold/recovery/removal arcs *)
    ; pulse_widths : (string * Pulse_width.t) list (** input pins that have any *)
    ; seq : Seq.t option
    }
  [@@deriving sexp_of]

  let pin_cap t pin = List.Assoc.find t.pin_cap pin ~equal:String.equal |> Option.value ~default:0.

  let arcs t ~output =
    List.Assoc.find t.arcs output ~equal:String.equal |> Option.value ~default:[]
  ;;

  let constraints t ~input =
    List.Assoc.find t.constraints input ~equal:String.equal |> Option.value ~default:[]
  ;;

  let pulse_width t ~input = List.Assoc.find t.pulse_widths input ~equal:String.equal
end

type t =
  { library : Cell_library.t
  ; cells : Cell.t Map.M(String).t
  ; default_input_pin_cap : float (** pF *)
  ; wire_loads : Wire_load.t list
  ; default_wire_load : string option
  ; wire_load_selection : (float * float * string) list
  (** the [default_wire_load_selection]: (min area, max area, model) *)
  }

(* --- Units ---------------------------------------------------------------- *)

let time_unit_ns lib =
  match Liberty.attr lib "time_unit" with
  | None -> 1.
  | Some s ->
    let s = String.lowercase (String.strip s) in
    let num, unit_ =
      String.lsplit2 s ~on:'n'
      |> Option.map ~f:(fun (n, _) -> n, 1.)
      |> Option.value_or_thunk ~default:(fun () ->
        match String.chop_suffix s ~suffix:"ps" with
        | Some n -> n, 1e-3
        | None ->
          (match String.chop_suffix s ~suffix:"us" with
           | Some n -> n, 1e3
           | None -> raise_s [%message "unsupported time_unit" (s : string)]))
    in
    Float.of_string num *. unit_
;;

let cap_unit_pf lib =
  match Liberty.complex lib "capacitive_load_unit" with
  | None -> 1.
  | Some [ n; u ] ->
    let n = Float.of_string (Liberty.value_to_string n) in
    (match String.lowercase (Liberty.value_to_string u) with
     | "pf" -> n
     | "ff" -> n *. 1e-3
     | u -> raise_s [%message "unsupported capacitive_load_unit" (u : string)])
  | Some _ -> raise_s [%message "bad capacitive_load_unit"]
;;

(* --- Tables --------------------------------------------------------------- *)

type template =
  { variable_1 : string option
  ; variable_2 : string option
  ; t_index_1 : float list option
  ; t_index_2 : float list option
  }

let templates lib =
  List.filter_map lib.Liberty.body ~f:(function
    | Liberty.Group ({ kind = "lu_table_template"; _ } as g) ->
      Some
        ( Liberty.group_name g
        , { variable_1 = Liberty.attr g "variable_1"
          ; variable_2 = Liberty.attr g "variable_2"
          ; t_index_1 = Option.map (Liberty.complex g "index_1") ~f:Liberty.floats
          ; t_index_2 = Option.map (Liberty.complex g "index_2") ~f:Liberty.floats
          } )
    | _ -> None)
  |> Map.of_alist_reduce (module String) ~f:(fun a _ -> a)
;;

let is_output_side = function
  | Some ("total_output_net_capacitance" | "related_pin_transition") -> true
  | _ -> false
;;

let scale_of_variable ~time ~cap = function
  | Some "total_output_net_capacitance" -> cap
  | Some ("input_net_transition" | "constrained_pin_transition" | "related_pin_transition") -> time
  | _ -> time
;;

let table ~templates ~time ~cap ~value_scale (g : Liberty.group) : Table.t =
  let tmpl =
    Map.find templates (Liberty.group_name g)
    |> Option.value
         ~default:{ variable_1 = None; variable_2 = None; t_index_1 = None; t_index_2 = None }
  in
  let index name tidx =
    match Liberty.complex g name with
    | Some vs -> Some (Liberty.floats vs)
    | None -> tidx
  in
  let index_1 = index "index_1" tmpl.t_index_1 in
  let index_2 = index "index_2" tmpl.t_index_2 in
  let rows =
    Liberty.complex g "values"
    |> Option.value_or_thunk ~default:(fun () ->
      raise_s [%message "table without values" (Liberty.group_name g : string)])
    |> Liberty.float_rows
  in
  let s1 = scale_of_variable ~time ~cap tmpl.variable_1 in
  let s2 = scale_of_variable ~time ~cap tmpl.variable_2 in
  let arr l ~scale = Array.of_list_map l ~f:(fun x -> x *. scale) in
  let values = Array.of_list_map rows ~f:(arr ~scale:value_scale) in
  (* raw: axis A indexes rows, axis B indexes columns *)
  let raw_a, raw_b, values =
    match index_1, index_2 with
    | Some i1, Some i2 -> arr i1 ~scale:s1, arr i2 ~scale:s2, values
    | Some i1, None ->
      (* one-dimensional: one row of values along index_1 *)
      let flat = Array.concat (Array.to_list values) in
      arr i1 ~scale:s1, [| 0. |], Array.map flat ~f:(fun v -> [| v |])
    | None, _ -> [| 0. |], [| 0. |], [| [| values.(0).(0) |] |]
  in
  let one_d_on_output = Option.is_none index_2 && is_output_side tmpl.variable_1 in
  let swap = Option.is_some index_2 && is_output_side tmpl.variable_1 in
  if one_d_on_output
  then
    { index_1 = [| 0. |]; index_2 = raw_a; values = [| Array.map values ~f:(fun r -> r.(0)) |] }
  else if swap
  then
    { index_1 = raw_b
    ; index_2 = raw_a
    ; values =
        Array.init (Array.length raw_b) ~f:(fun j ->
          Array.init (Array.length raw_a) ~f:(fun i -> values.(i).(j)))
    }
  else { index_1 = raw_a; index_2 = raw_b; values }
;;

(* --- Cells ---------------------------------------------------------------- *)

let parse_expr ~cell s =
  try Bool_expr.parse s with
  | Bool_expr.Parse_error msg -> raise_s [%message "bad liberty expression" (cell : string) (msg : string)]
;;

let arc ~templates ~time ~cap ~cell (g : Liberty.group) ~related_pin : Arc.t =
  let tbl kind ~value_scale =
    match Liberty.groups g kind with
    | [] -> None
    | t :: _ -> Some (table ~templates ~time ~cap ~value_scale t)
  in
  { related_pin
  ; timing_type = Option.value (Liberty.attr g "timing_type") ~default:"combinational"
  ; timing_sense = Liberty.attr g "timing_sense"
  ; when_ = Option.map (Liberty.attr g "when") ~f:(parse_expr ~cell)
  ; cell_rise = tbl "cell_rise" ~value_scale:time
  ; cell_fall = tbl "cell_fall" ~value_scale:time
  ; rise_transition = tbl "rise_transition" ~value_scale:time
  ; fall_transition = tbl "fall_transition" ~value_scale:time
  ; rise_constraint = tbl "rise_constraint" ~value_scale:time
  ; fall_constraint = tbl "fall_constraint" ~value_scale:time
  }
;;

let seq_of_group ~cell (g : Liberty.group) : Seq.t option =
  let expr name = Option.map (Liberty.attr g name) ~f:(parse_expr ~cell) in
  let names () =
    match List.map g.args ~f:Liberty.value_to_string with
    | [ q; qn ] -> q, qn
    | [ q ] -> q, q ^ "_N"
    | _ -> raise_s [%message "bad ff/latch group" (cell : string)]
  in
  let make kind =
    let state, state_n = names () in
    Some
      { Seq.state
      ; state_n
      ; kind
      ; clear = expr "clear"
      ; preset = expr "preset"
      ; clear_preset_var1 = Liberty.attr g "clear_preset_var1"
      }
  in
  let req name =
    match expr name with
    | Some e -> e
    | None -> raise_s [%message "missing attribute" (cell : string) (name : string)]
  in
  match g.kind with
  | "ff" -> make (Ff { clocked_on = req "clocked_on"; next_state = req "next_state" })
  | "latch" -> make (Latch { enable = req "enable"; data_in = req "data_in" })
  | _ -> None
;;

let cell_of_group ~templates ~time ~cap ~default_cap (logic : Cell_library.Cell.t) (g : Liberty.group)
  : Cell.t
  =
  let cell = logic.name in
  let pins = Liberty.groups g "pin" in
  let arcs_of p =
    List.concat_map (Liberty.groups p "timing") ~f:(fun tg ->
      let related =
        Option.value (Liberty.attr tg "related_pin") ~default:""
        |> String.split ~on:' '
        |> List.filter ~f:(fun s -> not (String.is_empty s))
      in
      List.map related ~f:(fun related_pin -> arc ~templates ~time ~cap ~cell tg ~related_pin))
  in
  let dir p = Liberty.attr p "direction" in
  let pin_cap =
    List.filter_map pins ~f:(fun p ->
      match dir p with
      | Some ("input" | "inout") ->
        let c =
          Option.value_map (Liberty.attr p "capacitance") ~default:default_cap ~f:(fun s ->
            Float.of_string s *. cap)
        in
        Some (Liberty.group_name p, c)
      | _ -> None)
  in
  let arcs =
    List.filter_map pins ~f:(fun p ->
      match dir p with
      | Some ("output" | "inout") ->
        Some
          ( Liberty.group_name p
          , List.filter (arcs_of p) ~f:(fun a ->
              (not (Arc.is_constraint a))
              && not
                   (List.mem [ "min_pulse_width"; "minimum_period" ] a.timing_type ~equal:String.equal))
          )
      | _ -> None)
  in
  let constraints =
    List.filter_map pins ~f:(fun p ->
      match dir p with
      | Some ("input" | "inout") ->
        (match List.filter (arcs_of p) ~f:Arc.is_constraint with
         | [] -> None
         | l -> Some (Liberty.group_name p, l))
      | _ -> None)
  in
  let pulse_widths =
    List.filter_map pins ~f:(fun p ->
      match dir p with
      | Some ("input" | "inout") ->
        let mpw = List.filter (arcs_of p) ~f:(fun a -> String.equal a.timing_type "min_pulse_width") in
        let const name =
          Option.map (Liberty.attr p name) ~f:(fun v ->
            { Table.index_1 = [| 0. |]; index_2 = [| 0. |]; values = [| [| Float.of_string v *. time |] |] })
        in
        let high =
          Option.first_some (const "min_pulse_width_high") (List.find_map mpw ~f:(fun a -> a.rise_constraint))
        in
        let low =
          Option.first_some (const "min_pulse_width_low") (List.find_map mpw ~f:(fun a -> a.fall_constraint))
        in
        if Option.is_none high && Option.is_none low
        then None
        else Some (Liberty.group_name p, { Pulse_width.high; low })
      | _ -> None)
  in
  let seq =
    List.find_map g.body ~f:(function
      | Liberty.Group ({ kind = "ff" | "latch"; _ } as sg) -> seq_of_group ~cell sg
      | _ -> None)
  in
  { name = cell; logic; pin_cap; arcs; constraints; pulse_widths; seq }
;;

let of_liberty (lib : Liberty.group) : t =
  let library = Cell_library.of_liberty lib in
  let time = time_unit_ns lib in
  let cap = cap_unit_pf lib in
  let templates = templates lib in
  let default_cap =
    Option.value_map (Liberty.attr lib "default_input_pin_cap") ~default:0. ~f:(fun s ->
      Float.of_string s *. cap)
  in
  let cells =
    List.map2_exn library.cells (Liberty.groups lib "cell") ~f:(fun logic g ->
      logic.name, cell_of_group ~templates ~time ~cap ~default_cap logic g)
    |> Map.of_alist_reduce (module String) ~f:(fun a _ -> a)
  in
  let wire_loads =
    List.map (Liberty.groups lib "wire_load") ~f:(fun g ->
      let num name = Option.value_map (Liberty.attr g name) ~default:0. ~f:Float.of_string in
      let fanout_length =
        List.filter_map g.body ~f:(function
          | Liberty.Complex ("fanout_length", vs) ->
            (match Liberty.floats vs with
             | f :: l :: _ -> Some (Float.iround_nearest_exn f, l)
             | _ -> raise_s [%message "bad fanout_length" (Liberty.group_name g : string)])
          | _ -> None)
        |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      in
      { Wire_load.name = Liberty.group_name g
      ; capacitance = num "capacitance" *. cap
      ; slope = num "slope"
      ; fanout_length
      })
  in
  let wire_load_selection =
    match Liberty.attr lib "default_wire_load_selection" with
    | None -> []
    | Some name ->
      (match
         List.find (Liberty.groups lib "wire_load_selection") ~f:(fun g ->
           String.equal (Liberty.group_name g) name)
       with
       | None -> raise_s [%message "default_wire_load_selection not found" (name : string)]
       | Some g ->
         List.filter_map g.body ~f:(function
           | Liberty.Complex ("wire_load_from_area", [ lo; hi; m ]) ->
             Some
               ( Float.of_string (Liberty.value_to_string lo)
               , Float.of_string (Liberty.value_to_string hi)
               , Liberty.value_to_string m )
           | _ -> None))
  in
  { library
  ; cells
  ; default_input_pin_cap = default_cap
  ; wire_loads
  ; default_wire_load = Liberty.attr lib "default_wire_load"
  ; wire_load_selection
  }
;;

let load path = of_liberty (Liberty.parse_file path)
let find t name = Map.find t.cells name

let find_exn t name =
  match find t name with
  | Some c -> c
  | None -> raise_s [%message "cell not in timing library" (name : string)]
;;

let find_wire_load t name =
  List.find t.wire_loads ~f:(fun (w : Wire_load.t) -> String.equal w.name name)
;;

let find_wire_load_exn t name =
  match find_wire_load t name with
  | Some w -> w
  | None -> raise_s [%message "no such wire_load in the timing library" (name : string)]
;;

(** The library's [default_wire_load]. *)
let default_wire_load t =
  match t.default_wire_load with
  | Some name -> find_wire_load_exn t name
  | None -> raise_s [%message "the timing library has no default_wire_load"]
;;

(** The model the [default_wire_load_selection] gives a design of [area]
    (in the library's cell-area units): as OpenSTA, the range with the
    greatest lower bound not above [area] (the first range if there is
    none; upper bounds are ignored). *)
let wire_load_for_area t area =
  match t.wire_load_selection with
  | [] -> raise_s [%message "the timing library has no default_wire_load_selection"]
  | (_, _, first) :: _ as sel ->
    let name =
      List.fold sel ~init:None ~f:(fun best (lo, _, m) ->
        match best with
        | _ when Float.(lo > area) -> best
        | Some (blo, _) when Float.(blo >= lo) -> best
        | _ -> Some (lo, m))
      |> Option.value_map ~default:first ~f:snd
    in
    find_wire_load_exn t name
;;
