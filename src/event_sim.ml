(* Event-driven, delay-aware gate-level simulation. Semantics are documented
   in event_sim.mli. *)

open! Base
open Hardcaml

(* --- Values ----------------------------------------------------------------- *)

module Value = struct
  type t =
    | V0
    | V1
    | X
  [@@deriving sexp_of, equal, compare]

  let to_char = function
    | V0 -> '0'
    | V1 -> '1'
    | X -> 'x'
  ;;

  let of_bool b = if b then V1 else V0
  let of_int = function
    | 0 -> V0
    | 1 -> V1
    | _ -> X
  ;;

  let to_int = function
    | V0 -> 0
    | V1 -> 1
    | X -> 2
  ;;
end

(* Internally values are ints: 0, 1, 2 = X. *)
let vx = 2
let v_not a = if a = vx then vx else 1 - a

let v_and a b =
  if a = 0 || b = 0 then 0 else if a = 1 && b = 1 then 1 else vx
;;

let v_or a b = if a = 1 || b = 1 then 1 else if a = 0 && b = 0 then 0 else vx
let v_xor a b = if a = vx || b = vx then vx else a lxor b

(* --- Time ------------------------------------------------------------------- *)

let fs_per_ns = 1_000_000
let of_ns x = Float.iround_nearest_exn (x *. Float.of_int fs_per_ns)
let of_ps x = Float.iround_nearest_exn (x *. 1000.)
let to_ns t = Float.of_int t /. Float.of_int fs_per_ns
let to_ps t = Float.of_int t /. 1000.

(* --- Configuration ---------------------------------------------------------- *)

module Config = struct
  type delay_mode =
    | Liberty
    | Zero
    | Unit of int
  [@@deriving sexp_of]

  type pulse_mode =
    | Inertial
    | Transport
  [@@deriving sexp_of]

  type init =
    | Init_x
    | Init_zero
  [@@deriving sexp_of]

  type t =
    { delay_mode : delay_mode
    ; pulse_mode : pulse_mode
    ; corner : float
    ; corner_scales_slew : bool
    ; variation_sigma : float
    ; seed : int
    ; wire_cap : float
    ; input_slew : float
    ; prim_delay : int
    ; reg_clock_to_q : int
    ; metastability : bool
    ; tau : float
    ; flop_init : init
    ; reg_init : init
    }
  [@@deriving sexp_of]

  let default =
    { delay_mode = Liberty
    ; pulse_mode = Inertial
    ; corner = 1.0
    ; corner_scales_slew = true
    ; variation_sigma = 0.
    ; seed = 0
    ; wire_cap = 0.001
    ; input_slew = 0.05
    ; prim_delay = 0
    ; reg_clock_to_q = 0
    ; metastability = true
    ; tau = 0.030
    ; flop_init = Init_x
    ; reg_init = Init_zero
    }
  ;;
end

(* --- Netlist ---------------------------------------------------------------- *)

type driver =
  { d_net : int
  ; transport : bool
  ; mutable pending : pending list (* ascending time *)
  }

and pending =
  { p_time : int
  ; p_value : int
  ; p_slew : float
  ; p_seq : int
  }

type arc_choice =
  { when_ : (int array -> int) option
  ; arc : Timing_library.Arc.t
  }

type seq_state =
  { spec : Timing_library.Seq.t
  ; trigger_fn : int array -> int (* clocked_on or enable *)
  ; data_fn : int array -> int (* next_state or data_in *)
  ; clear_fn : (int array -> int) option
  ; preset_fn : (int array -> int) option
  ; is_latch : bool
  ; clock_pin : int (* input index used for clock -> output arcs and constraints *)
  ; data_pins : (int * Timing_library.Arc.t list * Timing_library.Arc.t list) list
    (* input index, setup arcs, hold arcs *)
  ; mutable iq : int
  ; mutable prev_trigger : int
  ; mutable gen : int
  }

type cell_inst =
  { inst_name : string
  ; tcell : Timing_library.Cell.t
  ; in_pins : string array
  ; in_nets : int array
  ; out_pins : string array
  ; out_drivers : driver array
  ; out_fns : (int array -> int) array
  ; arcs : arc_choice list array array (* [output][input] *)
  ; any_arc : arc_choice list array (* [output] *)
  ; vals : int array (* inputs, then state, state_n *)
  ; factor : float
  ; seq : seq_state option
  }

type word_op =
  { w_inputs : int array
  ; w_outputs : driver array
  ; w_eval : int array -> int array
  ; w_buf : int array
  }

type reg_inst =
  { r_clock : int
  ; r_rising : bool
  ; r_d : int array
  ; r_enable : int option
  ; r_clear : (int * int array) option
  ; r_reset : (int * int * int array) option (* net, active level, reset_to *)
  ; r_state : int array
  ; r_drivers : driver array
  ; mutable r_prev_clock : int
  }

type element =
  | Cell_e of cell_inst
  | Word_e of word_op
  | Reg_e of reg_inst

module Metastable_event = struct
  type t =
    { instance : string
    ; edge_time : int
    ; resolve_time : int
    ; resolved_to : Value.t
    }
  [@@deriving sexp_of]
end

type event_kind =
  | Drive of driver
  | Input of int * int * float (* net, value, slew *)
  | Clock_toggle of clock_gen
  | Resolve of cell_inst * int * int (* gen, resolved value *)
  | Capture of cell_inst * int * int (* gen, edge time *)

and event =
  { time : int
  ; seq_no : int
  ; kind : event_kind
  }

and clock_gen =
  { c_net : int
  ; c_period : int
  ; c_high : int
  ; c_slew : float
  ; mutable c_level : int
  }

let hist_depth = 8

type t =
  { config : Config.t
  ; num_nets : int
  ; value : int array
  ; slew : float array
  ; load : float array
  ; hist_time : int array
  ; hist_value : int array
  ; hist_slew : float array
  ; hist_count : int array
  ; fanout : (int * int) array array (* net -> (element, input index) *)
  ; elements : element array
  ; names : (string, int list) Hashtbl.t
  ; buses : (string, int array) Hashtbl.t
  ; primary_inputs : (int, unit) Hashtbl.t
  ; watchers : (int -> int -> unit) list array
  ; meta_rng : Random.State.t
  ; mutable now : int
  ; mutable next_seq : int
  ; mutable heap : event array
  ; mutable heap_size : int
  ; mutable events_processed : int
  ; mutable same_instant : int
  ; mutable metastable : Metastable_event.t list
  ; mutable trace : (int * int * int) list option (* time, net, value; newest first *)
  }

(* --- Priority queue (binary heap on time, then sequence number) ------------- *)

let dummy_event = { time = 0; seq_no = 0; kind = Input (0, 0, 0.) }
let ev_lt a b = a.time < b.time || (a.time = b.time && a.seq_no < b.seq_no)

let heap_push t ev =
  if t.heap_size = Array.length t.heap
  then (
    let h = Array.create ~len:(2 * Array.length t.heap) dummy_event in
    Array.blit ~src:t.heap ~src_pos:0 ~dst:h ~dst_pos:0 ~len:t.heap_size;
    t.heap <- h);
  let h = t.heap in
  let i = ref t.heap_size in
  t.heap_size <- t.heap_size + 1;
  while !i > 0 && ev_lt ev h.((!i - 1) / 2) do
    h.(!i) <- h.((!i - 1) / 2);
    i := (!i - 1) / 2
  done;
  h.(!i) <- ev
;;

let heap_pop t =
  let h = t.heap in
  let top = h.(0) in
  t.heap_size <- t.heap_size - 1;
  let last = h.(t.heap_size) in
  h.(t.heap_size) <- dummy_event;
  if t.heap_size > 0
  then (
    let n = t.heap_size in
    let i = ref 0 in
    let continue = ref true in
    while !continue do
      let l = (2 * !i) + 1 in
      if l >= n
      then continue := false
      else (
        let c = if l + 1 < n && ev_lt h.(l + 1) h.(l) then l + 1 else l in
        if ev_lt h.(c) last
        then (
          h.(!i) <- h.(c);
          i := c)
        else continue := false)
    done;
    h.(!i) <- last);
  top
;;

let push t ~time kind =
  let seq_no = t.next_seq in
  t.next_seq <- seq_no + 1;
  heap_push t { time; seq_no; kind };
  seq_no
;;

(* --- Scheduling of driver outputs ------------------------------------------- *)

let schedule t (drv : driver) ~time ~value ~slew =
  let time = Int.max time t.now in
  if drv.transport
  then (
    drv.pending <- List.filter drv.pending ~f:(fun p -> p.p_time < time);
    let projected =
      match List.last drv.pending with
      | Some p -> p.p_value
      | None -> t.value.(drv.d_net)
    in
    if value <> projected
    then (
      let p_seq = push t ~time (Drive drv) in
      drv.pending <- drv.pending @ [ { p_time = time; p_value = value; p_slew = slew; p_seq } ]))
  else (
    match drv.pending with
    | [ p ] when p.p_value = value -> ()
    | _ ->
      drv.pending <- [];
      if value <> t.value.(drv.d_net)
      then (
        let p_seq = push t ~time (Drive drv) in
        drv.pending <- [ { p_time = time; p_value = value; p_slew = slew; p_seq } ]))
;;

(* --- Delay calculation ------------------------------------------------------ *)

let choose_arc (choices : arc_choice list) vals =
  match List.find choices ~f:(fun c -> match c.when_ with Some f -> f vals = 1 | None -> false) with
  | Some c -> Some c.arc
  | None ->
    (match List.find choices ~f:(fun c -> Option.is_none c.when_) with
     | Some c -> Some c.arc
     | None -> Option.map (List.hd choices) ~f:(fun c -> c.arc))
;;

let slew_factor t inst = if t.config.corner_scales_slew then t.config.corner *. inst.factor else 1.

(* Delay (fs) and output slew (ns) for output [o] of [inst] going to [value],
   caused by input [trig] (-1: unknown). *)
let cell_delay t inst ~o ~trig ~value =
  match t.config.delay_mode with
  | Zero -> 0, t.config.input_slew
  | Unit d -> d, t.config.input_slew
  | Liberty ->
    let choices =
      if trig >= 0
      then (
        match inst.arcs.(o).(trig) with
        | [] -> inst.any_arc.(o)
        | l -> l)
      else inst.any_arc.(o)
    in
    (match choose_arc choices inst.vals with
     | None -> 0, t.config.input_slew
     | Some arc ->
       let s_in =
         match arc.related_pin with
         | p when trig >= 0 && String.equal p inst.in_pins.(trig) -> t.slew.(inst.in_nets.(trig))
         | p ->
           (match Array.findi inst.in_pins ~f:(fun _ q -> String.equal p q) with
            | Some (i, _) -> t.slew.(inst.in_nets.(i))
            | None -> t.config.input_slew)
       in
       let load = t.load.(inst.out_drivers.(o).d_net) in
       let get tbl = Option.map tbl ~f:(fun tb -> Timing_library.Table.lookup tb s_in load) in
       let pick a b =
         match a, b with
         | Some a, _ -> a
         | None, Some b -> b
         | None, None -> 0.
       in
       let rise = pick (get arc.cell_rise) (get arc.cell_fall) in
       let fall = pick (get arc.cell_fall) (get arc.cell_rise) in
       let rise_s = pick (get arc.rise_transition) (get arc.fall_transition) in
       let fall_s = pick (get arc.fall_transition) (get arc.rise_transition) in
       let d, s =
         if value = 1
         then rise, rise_s
         else if value = 0
         then fall, fall_s
         else Float.min rise fall, Float.min rise_s fall_s
       in
       let f = t.config.corner *. inst.factor in
       of_ns (Float.max 0. d *. f), Float.max 0. s *. slew_factor t inst)
;;

(* --- Net updates ------------------------------------------------------------ *)

let record_history t net value slew =
  let c = t.hist_count.(net) in
  let k = (net * hist_depth) + (c % hist_depth) in
  t.hist_time.(k) <- t.now;
  t.hist_value.(k) <- value;
  t.hist_slew.(k) <- slew;
  t.hist_count.(net) <- c + 1
;;

(* Setup/hold aperture for a transition on data pin [i] going to [value] with
   slew [d_slew] (ns), relative to the edge: [(-setup, hold)] in fs. *)
let aperture t c (setup, hold) ~clock_slew ~value ~d_slew =
  let f = t.config.corner *. c.factor in
  let get arcs =
    List.fold arcs ~init:None ~f:(fun acc (a : Timing_library.Arc.t) ->
      let tbl =
        if value = 1
        then a.rise_constraint
        else if value = 0
        then a.fall_constraint
        else Option.first_some a.rise_constraint a.fall_constraint
      in
      match tbl, acc with
      | Some tb, None -> Some (Timing_library.Table.lookup tb d_slew clock_slew)
      | Some tb, Some x -> Some (Float.max x (Timing_library.Table.lookup tb d_slew clock_slew))
      | None, acc -> acc)
  in
  let s = Option.value (get setup) ~default:0. in
  let h = Option.value (get hold) ~default:0. in
  of_ns (-.s *. f), of_ns (h *. f)
;;

let rec capture t c s ~edge_time =
  let liberty =
    match t.config.delay_mode with
    | Liberty -> true
    | Zero | Unit _ -> false
  in
  if (not t.config.metastability) || (not liberty) || List.is_empty s.data_pins
  then (
    s.gen <- s.gen + 1;
    s.iq <- s.data_fn c.vals)
  else (
    let clock_slew = t.slew.(c.in_nets.(s.clock_pin)) in
    let apertures =
      List.map s.data_pins ~f:(fun (i, setup, hold) ->
        let net = c.in_nets.(i) in
        let ap v = aperture t c (setup, hold) ~clock_slew ~value:v ~d_slew:t.slew.(net) in
        snd (ap 1), snd (ap 0))
    in
    let max_hold =
      List.fold apertures ~init:0 ~f:(fun acc (h1, h0) -> Int.max acc (Int.max h1 h0))
    in
    if edge_time + max_hold > t.now
    then (
      (* hold window still open: decide later *)
      s.gen <- s.gen + 1;
      ignore (push t ~time:(edge_time + max_hold) (Capture (c, s.gen, edge_time)) : int))
    else decide_capture t c s ~edge_time ~clock_slew)

and decide_capture t c s ~edge_time ~clock_slew =
  (* Each recorded transition of a data pin is, relative to its own
     (direction- and slew-dependent) aperture (lo, hi) around the edge:
     before it (captured), after it (not captured) or inside (violation).
     The captured value is the one set by the last transition before its
     aperture; without one in the history, the value before the oldest
     recorded transition (X if the history does not reach back far enough). *)
  let violation = ref false in
  let n = Array.length c.in_nets in
  let saved = Array.sub c.vals ~pos:0 ~len:n in
  for i = 0 to n - 1 do
    c.vals.(i) <- t.value.(c.in_nets.(i))
  done;
  List.iter s.data_pins ~f:(fun (i, setup, hold) ->
    let net = c.in_nets.(i) in
    let cnt = t.hist_count.(net) in
    let first = Int.max 0 (cnt - hist_depth) in
    let captured = ref vx in
    for k = first to cnt - 1 do
      let idx = (net * hist_depth) + (k % hist_depth) in
      let td = t.hist_time.(idx) - edge_time in
      let lo, hi =
        aperture
          t
          c
          (setup, hold)
          ~clock_slew
          ~value:t.hist_value.(idx)
          ~d_slew:t.hist_slew.(idx)
      in
      let lo, hi = if lo < hi then lo, hi else (lo + hi) / 2, (lo + hi) / 2 in
      if td > lo && td < hi then violation := true else if td <= lo then captured := t.hist_value.(idx)
    done;
    c.vals.(i) <- !captured);
  let next = s.data_fn c.vals in
  Array.blit ~src:saved ~src_pos:0 ~dst:c.vals ~dst_pos:0 ~len:n;
  s.gen <- s.gen + 1;
  if !violation
  then (
    s.iq <- vx;
    let u = Random.State.float t.meta_rng 1.0 in
    let dt = of_ns (-.t.config.tau *. Float.log (1. -. u)) in
    let resolved = Random.State.int t.meta_rng 2 in
    let resolve_time = edge_time + dt in
    t.metastable
    <- { instance = c.inst_name
       ; edge_time
       ; resolve_time
       ; resolved_to = Value.of_int resolved
       }
       :: t.metastable;
    ignore (push t ~time:resolve_time (Resolve (c, s.gen, resolved)) : int))
  else s.iq <- next
;;

let rec set_net t net value slew =
  if t.value.(net) <> value
  then (
    t.value.(net) <- value;
    t.slew.(net) <- slew;
    record_history t net value slew;
    (match t.trace with
     | Some l -> t.trace <- Some ((t.now, net, value) :: l)
     | None -> ());
    List.iter t.watchers.(net) ~f:(fun f -> f t.now value);
    Array.iter t.fanout.(net) ~f:(fun (e, i) -> eval t t.elements.(e) ~trig:i))

and eval t e ~trig =
  match e with
  | Word_e w -> eval_word t w
  | Reg_e r -> eval_reg t r
  | Cell_e c -> eval_cell t c ~trig

and eval_word t w =
  Array.iteri w.w_inputs ~f:(fun i n -> w.w_buf.(i) <- t.value.(n));
  let out = w.w_eval w.w_buf in
  Array.iteri w.w_outputs ~f:(fun i drv ->
    schedule t drv ~time:(t.now + t.config.prim_delay) ~value:out.(i) ~slew:t.config.input_slew)

and eval_reg t r =
  let v n = t.value.(n) in
  let c = v r.r_clock in
  (* last known clock level; X at power-up, so the first resolution of the
     clock net is not an edge *)
  let c_old = r.r_prev_clock in
  if c <> vx then r.r_prev_clock <- c;
  let set_bits f =
    Array.iteri r.r_state ~f:(fun i q ->
      let nv = f i q in
      if nv <> q
      then (
        r.r_state.(i) <- nv;
        schedule
          t
          r.r_drivers.(i)
          ~time:(t.now + t.config.reg_clock_to_q)
          ~value:nv
          ~slew:t.config.input_slew))
  in
  let merge a b = if a = b then a else vx in
  let next () =
    (* value loaded on an active edge, without reset *)
    let load_d i q =
      match r.r_enable with
      | None -> v r.r_d.(i)
      | Some en ->
        let en = v en in
        if en = 1 then v r.r_d.(i) else if en = 0 then q else merge q (v r.r_d.(i))
    in
    match r.r_clear with
    | None -> load_d
    | Some (clr, clear_to) ->
      fun i q ->
        let cl = v clr in
        if cl = 1
        then v clear_to.(i)
        else if cl = 0
        then load_d i q
        else merge (v clear_to.(i)) (load_d i q)
  in
  let reset_state =
    match r.r_reset with
    | None -> `Inactive
    | Some (rst, level, reset_to) ->
      let rv = v rst in
      if rv = vx then `Unknown reset_to else if rv = level then `Active reset_to else `Inactive
  in
  match reset_state with
  | `Active reset_to -> set_bits (fun i _ -> v reset_to.(i))
  | `Unknown reset_to -> set_bits (fun i q -> merge q (v reset_to.(i)))
  | `Inactive ->
    let active_old, active_new = if r.r_rising then 0, 1 else 1, 0 in
    if c_old = active_old && c = active_new
    then (
      let f = next () in
      set_bits f)
    else if c = vx && c_old = active_old
    then (
      let f = next () in
      set_bits (fun i q -> merge q (f i q)))

and eval_cell t c ~trig =
  let n = Array.length c.in_nets in
  for i = 0 to n - 1 do
    c.vals.(i) <- t.value.(c.in_nets.(i))
  done;
  (match c.seq with
   | None -> ()
   | Some s -> update_seq t c s);
  eval_cell_outputs t c ~trig ~base:t.now

and eval_cell_outputs t c ~trig ~base =
  let n = Array.length c.in_nets in
  (match c.seq with
   | None -> ()
   | Some s ->
     c.vals.(n) <- s.iq;
     c.vals.(n + 1) <- v_not s.iq);
  Array.iteri c.out_fns ~f:(fun o f ->
    let value = f c.vals in
    let drv = c.out_drivers.(o) in
    let projected =
      match List.last drv.pending with
      | Some p -> p.p_value
      | None -> t.value.(drv.d_net)
    in
    if value <> projected
    then (
      let d, slew = cell_delay t c ~o ~trig ~value in
      schedule t drv ~time:(base + d) ~value ~slew))

and update_seq t c s =
  let vals = c.vals in
  let tr = s.trigger_fn vals in
  (* last known level, as for registers *)
  let tr_old = s.prev_trigger in
  if tr <> vx then s.prev_trigger <- tr;
  let clr = Option.value_map s.clear_fn ~default:0 ~f:(fun f -> f vals) in
  let pre = Option.value_map s.preset_fn ~default:0 ~f:(fun f -> f vals) in
  let set_iq v =
    s.gen <- s.gen + 1;
    s.iq <- v
  in
  if clr = 1 && pre = 1
  then
    set_iq
      (match s.spec.clear_preset_var1 with
       | Some "L" -> 0
       | Some "H" -> 1
       | _ -> vx)
  else if clr = 1
  then set_iq 0
  else if pre = 1
  then set_iq 1
  else if clr = vx || pre = vx
  then (
    let target = if clr = vx then 0 else 1 in
    if s.iq <> target then set_iq vx)
  else if s.is_latch
  then
    if tr = 1
    then set_iq (s.data_fn vals)
    else if tr_old = 1 && tr = 0
    then capture t c s ~edge_time:t.now
    else if tr = vx
    then (if s.data_fn vals <> s.iq then set_iq vx)
    else ()
  else if tr_old = 0 && tr = 1
  then capture t c s ~edge_time:t.now
  else if tr = vx && tr_old = 0
  then if s.data_fn vals <> s.iq then set_iq vx
;;


(* --- Event processing ------------------------------------------------------- *)

let refresh_vals t c =
  for i = 0 to Array.length c.in_nets - 1 do
    c.vals.(i) <- t.value.(c.in_nets.(i))
  done
;;

let max_events_per_instant = 1_000_000

let process t ev =
  if ev.time = t.now
  then (
    t.same_instant <- t.same_instant + 1;
    if t.same_instant > max_events_per_instant
    then
      raise_s
        [%message
          "Event_sim: more than a million events at one instant (a loop with zero delay?)"
            ~time:(ev.time : int)])
  else t.same_instant <- 0;
  t.now <- ev.time;
  t.events_processed <- t.events_processed + 1;
  match ev.kind with
  | Drive drv ->
    (match drv.pending with
     | p :: rest when p.p_seq = ev.seq_no ->
       drv.pending <- rest;
       set_net t drv.d_net p.p_value p.p_slew
     | _ -> ())
  | Input (net, v, slew) -> set_net t net v slew
  | Clock_toggle g ->
    let v = if g.c_level = 1 then 0 else 1 in
    g.c_level <- v;
    set_net t g.c_net v g.c_slew;
    let next = if v = 1 then g.c_high else g.c_period - g.c_high in
    ignore (push t ~time:(t.now + next) (Clock_toggle g) : int)
  | Resolve (c, gen, v) ->
    (match c.seq with
     | Some s when s.gen = gen ->
       s.iq <- v;
       refresh_vals t c;
       eval_cell_outputs t c ~trig:s.clock_pin ~base:t.now
     | _ -> ())
  | Capture (c, gen, edge_time) ->
    (match c.seq with
     | Some s when s.gen = gen ->
       refresh_vals t c;
       decide_capture t c s ~edge_time ~clock_slew:t.slew.(c.in_nets.(s.clock_pin));
       refresh_vals t c;
       eval_cell_outputs t c ~trig:s.clock_pin ~base:edge_time
     | _ -> ())
;;

let run_until t time =
  while t.heap_size > 0 && t.heap.(0).time <= time do
    process t (heap_pop t)
  done;
  if time > t.now then t.now <- time
;;

let run_for t d = run_until t (t.now + d)

let run_until_quiescent ?(limit = Int.max_value) t =
  while t.heap_size > 0 && t.heap.(0).time <= limit do
    process t (heap_pop t)
  done;
  t.heap_size = 0
;;

(* --- Netlist extraction ----------------------------------------------------- *)

let rec compile (e : Bool_expr.t) ~idx : int array -> int =
  match e with
  | Const b ->
    let v = if b then 1 else 0 in
    fun _ -> v
  | Var v ->
    let i = idx v in
    fun a -> a.(i)
  | Not a ->
    let f = compile a ~idx in
    fun x -> v_not (f x)
  | And (a, b) ->
    let f = compile a ~idx
    and g = compile b ~idx in
    fun x -> v_and (f x) (g x)
  | Or (a, b) ->
    let f = compile a ~idx
    and g = compile b ~idx in
    fun x -> v_or (f x) (g x)
  | Xor (a, b) ->
    let f = compile a ~idx
    and g = compile b ~idx in
    fun x -> v_xor (f x) (g x)
;;

let bits_of (a : int array) ~pos ~width =
  Bits.of_string (String.init width ~f:(fun k -> if a.(pos + width - 1 - k) = 1 then '1' else '0'))
;;

let to_array (b : Bits.t) =
  let s = Bits.to_bstr b in
  let w = String.length s in
  Array.init w ~f:(fun i -> if Char.equal s.[w - 1 - i] '1' then 1 else 0)
;;

let any_x (a : int array) ~pos ~width =
  let r = ref false in
  for i = pos to pos + width - 1 do
    if a.(i) = vx then r := true
  done;
  !r
;;

let gaussian rng =
  let u1 = Float.max 1e-300 (Random.State.float rng 1.0) in
  let u2 = Random.State.float rng 1.0 in
  Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)
;;

exception Unsupported of Sexp.t

let unsupported s = raise (Unsupported s)

let create ?(config = Config.default) (lib : Timing_library.t) (circuit : Circuit.t) =
  let graph = Circuit.signal_graph circuit in
  let signals = Signal_graph.fold graph ~init:[] ~f:(fun acc s -> s :: acc) |> List.rev in
  let signals = List.filter signals ~f:(fun s -> not (Signal.Type.is_empty s)) in
  (* one raw net per signal bit *)
  let base = Hashtbl.create (module Signal.Type.Uid) in
  let nraw = ref 0 in
  List.iter signals ~f:(fun s ->
    Hashtbl.set base ~key:(Signal.uid s) ~data:!nraw;
    nraw := !nraw + Signal.width s);
  let raw s i =
    match Hashtbl.find base (Signal.uid s) with
    | Some b -> b + i
    | None -> unsupported [%message "signal outside the circuit graph" (Signal.uid s : Signal.Type.Uid.t)]
  in
  (* union-find: wires, selects and concatenations only rename bits *)
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
  let canon = Array.create ~len:!nraw (-1) in
  let num_nets = ref 0 in
  for i = 0 to !nraw - 1 do
    let r = find i in
    if canon.(r) < 0
    then (
      canon.(r) <- !num_nets;
      Int.incr num_nets)
  done;
  let num_nets = !num_nets in
  let net s i = canon.(find (raw s i)) in
  let nets s = Array.init (Signal.width s) ~f:(fun i -> net s i) in
  (* drivers *)
  let driven = Array.create ~len:num_nets "" in
  let pulse_transport =
    match config.pulse_mode with
    | Transport -> true
    | Inertial -> false
  in
  let new_driver ~who ~transport n =
    if not (String.is_empty driven.(n))
    then unsupported [%message "net has several drivers" (who : string) ~other:(driven.(n) : string)];
    driven.(n) <- who;
    { d_net = n; transport; pending = [] }
  in
  let what s =
    match Signal.names s with
    | [] -> "<unnamed>"
    | names -> String.concat ~sep:"," names
  in
  let elements = Queue.create () in
  let consts = Queue.create () in
  let regs_init = Queue.create () in
  let primary_inputs = Hashtbl.create (module Int) in
  let var_rng = Random.State.make [| config.seed; 1 |] in
  let cells = Queue.create () in
  List.iter signals ~f:(fun s ->
    match s with
    | Signal.Type.Wire { driver = None; _ } ->
      if Circuit.is_input circuit s
      then Array.iter (nets s) ~f:(fun n -> Hashtbl.set primary_inputs ~key:n ~data:())
      else unsupported [%message "undriven wire" ~signal:(what s : string)]
    | Wire _ | Select _ | Cat _ -> ()
    | Empty -> ()
    | Const { constant; _ } ->
      let bits = to_array constant in
      Array.iteri (nets s) ~f:(fun i n ->
        Queue.enqueue consts (new_driver ~who:"constant" ~transport:false n, bits.(i)))
    | Not { arg; _ } ->
      let w = Signal.width s in
      Queue.enqueue
        elements
        (Word_e
           { w_inputs = nets arg
           ; w_outputs = Array.map (nets s) ~f:(new_driver ~who:"not" ~transport:pulse_transport)
           ; w_eval = (fun a -> Array.init w ~f:(fun i -> v_not a.(i)))
           ; w_buf = Array.create ~len:w 0
           })
    | Op2 { op; arg_a; arg_b; _ } ->
      let wa = Signal.width arg_a
      and wb = Signal.width arg_b
      and w = Signal.width s in
      let inputs = Array.append (nets arg_a) (nets arg_b) in
      let bitwise f a = Array.init w ~f:(fun i -> f a.(i) a.(wa + i)) in
      let arith f a =
        if any_x a ~pos:0 ~width:(wa + wb)
        then Array.create ~len:w vx
        else to_array (f (bits_of a ~pos:0 ~width:wa) (bits_of a ~pos:wa ~width:wb))
      in
      let eval : int array -> int array =
        match op with
        | And -> bitwise v_and
        | Or -> bitwise v_or
        | Xor -> bitwise v_xor
        | Add -> arith Bits.( +: )
        | Sub -> arith Bits.( -: )
        | Mulu -> arith Bits.( *: )
        | Muls -> arith Bits.( *+ )
        | Eq -> arith Bits.( ==: )
        | Lt -> arith Bits.( <: )
      in
      Queue.enqueue
        elements
        (Word_e
           { w_inputs = inputs
           ; w_outputs = Array.map (nets s) ~f:(new_driver ~who:"op2" ~transport:pulse_transport)
           ; w_eval = eval
           ; w_buf = Array.create ~len:(wa + wb) 0
           })
    | Mux { select; cases; _ } ->
      let ws = Signal.width select
      and w = Signal.width s in
      let ncases = List.length cases in
      let inputs = Array.concat (nets select :: List.map cases ~f:nets) in
      let eval a =
        if any_x a ~pos:0 ~width:ws
        then
          Array.init w ~f:(fun i ->
            let v0 = a.(ws + i) in
            if List.for_all (List.init ncases ~f:Fn.id) ~f:(fun c -> a.(ws + (c * w) + i) = v0)
            then v0
            else vx)
        else (
          let sel = Bits.to_int_trunc (bits_of a ~pos:0 ~width:ws) in
          let c = if sel < 0 || sel >= ncases then ncases - 1 else sel in
          Array.init w ~f:(fun i -> a.(ws + (c * w) + i)))
      in
      Queue.enqueue
        elements
        (Word_e
           { w_inputs = inputs
           ; w_outputs = Array.map (nets s) ~f:(new_driver ~who:"mux" ~transport:pulse_transport)
           ; w_eval = eval
           ; w_buf = Array.create ~len:(Array.length inputs) 0
           })
    | Cases { select; cases; default; _ } ->
      let ws = Signal.width select
      and w = Signal.width s in
      let inputs =
        Array.concat
          ((nets select :: List.concat_map cases ~f:(fun (m, d) -> [ nets m; nets d ]))
           @ [ nets default ])
      in
      let ncases = List.length cases in
      let eval a =
        let stride = ws + w in
        if any_x a ~pos:0 ~width:ws
           || List.exists (List.init ncases ~f:Fn.id) ~f:(fun c ->
             any_x a ~pos:(ws + (c * stride)) ~width:ws)
        then Array.create ~len:w vx
        else (
          let sel = bits_of a ~pos:0 ~width:ws in
          let pos =
            match
              List.find (List.init ncases ~f:Fn.id) ~f:(fun c ->
                Bits.equal sel (bits_of a ~pos:(ws + (c * stride)) ~width:ws))
            with
            | Some c -> ws + (c * stride) + ws
            | None -> ws + (ncases * stride)
          in
          Array.init w ~f:(fun i -> a.(pos + i)))
      in
      Queue.enqueue
        elements
        (Word_e
           { w_inputs = inputs
           ; w_outputs = Array.map (nets s) ~f:(new_driver ~who:"cases" ~transport:pulse_transport)
           ; w_eval = eval
           ; w_buf = Array.create ~len:(Array.length inputs) 0
           })
    | Reg { register; d; _ } ->
      let w = Signal.width s in
      let init =
        match register.initialize_to with
        | Some b -> to_array b
        | None ->
          Array.create
            ~len:w
            (match config.reg_init with
             | Init_x -> vx
             | Init_zero -> 0)
      in
      let r =
        { r_clock = net register.clock.clock 0
        ; r_rising =
            (match register.clock.clock_edge with
             | Rising -> true
             | Falling -> false)
        ; r_d = nets d
        ; r_enable = Option.map register.enable ~f:(fun e -> net e 0)
        ; r_clear = Option.map register.clear ~f:(fun c -> net c.clear 0, nets c.clear_to)
        ; r_reset =
            Option.map register.reset ~f:(fun r ->
              ( net r.reset 0
              , (match r.reset_level with
                 | High -> 1
                 | Low -> 0)
              , nets r.reset_to ))
        ; r_state = Array.copy init
        ; r_drivers = Array.map (nets s) ~f:(new_driver ~who:"reg" ~transport:true)
        ; r_prev_clock = vx
        }
      in
      Queue.enqueue regs_init (r, init);
      Queue.enqueue elements (Reg_e r)
    | Multiport_mem _ | Mem_read_port _ ->
      unsupported [%message "memories are not supported by Event_sim" ~signal:(what s : string)]
    | Inst { instantiation = inst; _ } ->
      let name = inst.circuit_name in
      let tcell =
        match Timing_library.find lib name with
        | Some c -> c
        | None ->
          unsupported
            [%message
              "instantiation is not a cell of the timing library (hierarchical \
               instantiations are not supported)"
                (name : string)
                ~instance:(inst.instance_label : string)]
      in
      let in_pins = Array.of_list_map inst.inputs ~f:(fun i -> i.name) in
      let in_nets =
        Array.of_list_map inst.inputs ~f:(fun i ->
          if Signal.width i.input_signal <> 1
          then
            unsupported
              [%message "cell pins must be 1 bit" ~instance:(inst.instance_label : string) i.name];
          net i.input_signal 0)
      in
      let n = Array.length in_pins in
      let seq = tcell.seq in
      let idx v =
        match Array.findi in_pins ~f:(fun _ p -> String.equal p v) with
        | Some (i, _) -> i
        | None ->
          (match seq with
           | Some sq when String.equal v sq.state -> n
           | Some sq when String.equal v sq.state_n -> n + 1
           | _ ->
             unsupported
               [%message "unknown pin in cell function" (name : string) ~pin:(v : string)])
      in
      let logic = tcell.logic in
      let outs =
        List.filter_map inst.outputs ~f:(fun o ->
          if o.output_width <> 1
          then unsupported [%message "cell pins must be 1 bit" (name : string) o.name];
          match Cell_library.Cell.pin logic o.name with
          | None -> unsupported [%message "unknown output pin" (name : string) o.name]
          | Some p ->
            let f =
              match p.function_ with
              | Some e -> compile e ~idx
              | None -> fun _ -> vx
            in
            let f =
              match p.three_state with
              | None -> f
              | Some ts ->
                let tsf = compile ts ~idx in
                fun a -> if tsf a = 0 then f a else vx
            in
            Some (o.name, net s o.output_low_index, f))
      in
      let out_pins = Array.of_list_map outs ~f:(fun (p, _, _) -> p) in
      let choice (a : Timing_library.Arc.t) =
        { when_ = Option.map a.when_ ~f:(compile ~idx); arc = a }
      in
      let arcs =
        Array.map out_pins ~f:(fun o ->
          let all = Timing_library.Cell.arcs tcell ~output:o in
          Array.map in_pins ~f:(fun p ->
            List.filter all ~f:(fun a -> String.equal a.related_pin p) |> List.map ~f:choice))
      in
      let any_arc =
        Array.map out_pins ~f:(fun o -> List.map (Timing_library.Cell.arcs tcell ~output:o) ~f:choice)
      in
      let seq =
        Option.map seq ~f:(fun (sq : Timing_library.Seq.t) ->
          let trigger, data, is_latch =
            match sq.kind with
            | Ff { clocked_on; next_state } -> clocked_on, next_state, false
            | Latch { enable; data_in } -> enable, data_in, true
          in
          let clock_name =
            match Bool_expr.vars trigger with
            | v :: _ -> v
            | [] -> unsupported [%message "constant clock" (name : string)]
          in
          let clock_pin = idx clock_name in
          let data_pins =
            Array.to_list in_pins
            |> List.filter_mapi ~f:(fun i p ->
              let cs =
                Timing_library.Cell.constraints tcell ~input:p
                |> List.filter ~f:(fun (a : Timing_library.Arc.t) ->
                  String.equal a.related_pin clock_name)
              in
              let setup = List.filter cs ~f:(fun a -> String.is_prefix a.timing_type ~prefix:"setup_") in
              let hold = List.filter cs ~f:(fun a -> String.is_prefix a.timing_type ~prefix:"hold_") in
              if List.is_empty setup && List.is_empty hold then None else Some (i, setup, hold))
          in
          { spec = sq
          ; trigger_fn = compile trigger ~idx
          ; data_fn = compile data ~idx
          ; clear_fn = Option.map sq.clear ~f:(compile ~idx)
          ; preset_fn = Option.map sq.preset ~f:(compile ~idx)
          ; is_latch
          ; clock_pin
          ; data_pins
          ; iq =
              (match config.flop_init with
               | Init_x -> vx
               | Init_zero -> 0)
          ; prev_trigger = vx
          ; gen = 0
          })
      in
      let transport = pulse_transport || Option.is_some seq in
      let who = inst.instance_label in
      Queue.enqueue
        cells
        { inst_name = inst.instance_label
        ; tcell
        ; in_pins
        ; in_nets
        ; out_pins
        ; out_drivers =
            Array.of_list_map outs ~f:(fun (p, n, _) ->
              new_driver ~who:(who ^ "/" ^ p) ~transport n)
        ; out_fns = Array.of_list_map outs ~f:(fun (_, _, f) -> f)
        ; arcs
        ; any_arc
        ; vals = Array.create ~len:(n + 2) vx
        ; factor = 1.
        ; seq
        });
  (* per-instance variation, drawn in instance-name order so it is stable *)
  let cells =
    Queue.to_list cells
    |> List.sort ~compare:(fun a b -> String.compare a.inst_name b.inst_name)
    |> List.map ~f:(fun c ->
      if Float.( > ) config.variation_sigma 0.
      then { c with factor = Float.max 0.05 (1. +. (config.variation_sigma *. gaussian var_rng)) }
      else c)
  in
  List.iter cells ~f:(fun c -> Queue.enqueue elements (Cell_e c));
  let elements = Queue.to_array elements in
  (* fanout and load *)
  let fanout = Array.create ~len:num_nets [] in
  let load = Array.create ~len:num_nets config.wire_cap in
  Array.iteri elements ~f:(fun e el ->
    let add_unique ns =
      Array.to_list ns
      |> List.dedup_and_sort ~compare:Int.compare
      |> List.iter ~f:(fun n -> fanout.(n) <- (e, -1) :: fanout.(n))
    in
    match el with
    | Cell_e c ->
      Array.iteri c.in_nets ~f:(fun i n ->
        fanout.(n) <- (e, i) :: fanout.(n);
        load.(n) <- load.(n) +. Timing_library.Cell.pin_cap c.tcell c.in_pins.(i))
    | Word_e w -> add_unique w.w_inputs
    | Reg_e r ->
      add_unique
        (Array.concat
           ([ [| r.r_clock |]; r.r_d ]
            @ Option.value_map r.r_enable ~default:[] ~f:(fun e -> [ [| e |] ])
            @ Option.value_map r.r_clear ~default:[] ~f:(fun (c, ct) -> [ [| c |]; ct ])
            @ Option.value_map r.r_reset ~default:[] ~f:(fun (c, _, rt) -> [ [| c |]; rt ]))));
  let fanout = Array.map fanout ~f:(fun l -> Array.of_list (List.rev l)) in
  (* names *)
  let names = Hashtbl.create (module String) in
  let buses = Hashtbl.create (module String) in
  let add_name name n =
    Hashtbl.update names name ~f:(function
      | None -> [ n ]
      | Some l -> if List.mem l n ~equal:Int.equal then l else l @ [ n ])
  in
  List.iter signals ~f:(fun s ->
    let w = Signal.width s in
    List.iter (Signal.names s) ~f:(fun name ->
      if w = 1
      then add_name name (net s 0)
      else if w > 1
      then (
        Hashtbl.set buses ~key:name ~data:(nets s);
        for i = 0 to w - 1 do
          add_name (Printf.sprintf "%s[%d]" name i) (net s i)
        done)));
  List.iter cells ~f:(fun c ->
    Array.iteri c.out_pins ~f:(fun o p -> add_name (c.inst_name ^ "/" ^ p) c.out_drivers.(o).d_net);
    Array.iteri c.in_pins ~f:(fun i p -> add_name (c.inst_name ^ "/" ^ p) c.in_nets.(i)));
  let t =
    { config
    ; num_nets
    ; value = Array.create ~len:num_nets vx
    ; slew = Array.create ~len:num_nets config.input_slew
    ; load
    ; hist_time = Array.create ~len:(num_nets * hist_depth) 0
    ; hist_value = Array.create ~len:(num_nets * hist_depth) vx
    ; hist_slew = Array.create ~len:(num_nets * hist_depth) 0.
    ; hist_count = Array.create ~len:num_nets 0
    ; fanout
    ; elements
    ; names
    ; buses
    ; primary_inputs
    ; watchers = Array.create ~len:num_nets []
    ; meta_rng = Random.State.make [| config.seed; 2 |]
    ; now = 0
    ; next_seq = 0
    ; heap = Array.create ~len:1024 dummy_event
    ; heap_size = 0
    ; events_processed = 0
    ; same_instant = 0
    ; metastable = []
    ; trace = None
    }
  in
  (* time 0: constants, register initial values, then every element once *)
  Queue.iter consts ~f:(fun (drv, v) -> schedule t drv ~time:0 ~value:v ~slew:config.input_slew);
  Queue.iter regs_init ~f:(fun (r, init) ->
    Array.iteri init ~f:(fun i v ->
      if v <> vx then schedule t r.r_drivers.(i) ~time:0 ~value:v ~slew:config.input_slew));
  Array.iter elements ~f:(fun e ->
    match e with
    | Reg_e _ -> ()
    | Cell_e c -> eval_cell t c ~trig:(-1)
    | Word_e _ -> eval t e ~trig:(-1));
  t
;;

(* --- Public API ------------------------------------------------------------- *)

let now t = t.now
let events_processed t = t.events_processed
let num_nets t = t.num_nets
let config t = t.config

let net_exn t name =
  match Hashtbl.find t.names name with
  | Some [ n ] -> n
  | Some (_ :: _ as l) ->
    raise_s [%message "ambiguous net name" (name : string) ~nets:(List.length l : int)]
  | Some [] | None -> raise_s [%message "no such net" (name : string)]
;;

let has_net t name = Hashtbl.mem t.names name
let names t = Hashtbl.keys t.names |> List.sort ~compare:String.compare
let value t name = Value.of_int t.value.(net_exn t name)
let slew_ns t name = t.slew.(net_exn t name)
let load_pf t name = t.load.(net_exn t name)

let bus_exn t name =
  match Hashtbl.find t.buses name with
  | Some b -> b
  | None -> [| net_exn t name |]
;;

let values t name = Array.map (bus_exn t name) ~f:(fun n -> Value.of_int t.value.(n))

let to_int t name =
  let b = bus_exn t name in
  Array.foldi b ~init:(Some 0) ~f:(fun i acc n ->
    match acc with
    | None -> None
    | Some acc ->
      let v = t.value.(n) in
      if v = vx then None else Some (acc lor (v lsl i)))
;;

let check_input t n name =
  if not (Hashtbl.mem t.primary_inputs n)
  then raise_s [%message "not a primary input" (name : string)]
;;

let check_time t at =
  if at < t.now then raise_s [%message "cannot schedule in the past" (at : int) ~now:(t.now : int)]
;;

let set_input ?slew t name (v : Value.t) ~at =
  let n = net_exn t name in
  check_input t n name;
  check_time t at;
  let slew = Option.value slew ~default:t.config.input_slew in
  ignore (push t ~time:at (Input (n, Value.to_int v, slew)) : int)
;;

let set_input_int ?slew t name x ~at =
  let b = bus_exn t name in
  check_time t at;
  let slew = Option.value slew ~default:t.config.input_slew in
  Array.iteri b ~f:(fun i n ->
    check_input t n name;
    ignore (push t ~time:at (Input (n, (x lsr i) land 1, slew)) : int))
;;

let add_clock ?(start = 0) ?high ?slew t name ~period =
  let n = net_exn t name in
  check_input t n name;
  let high = Option.value high ~default:(period / 2) in
  if period <= 0 || high <= 0 || high >= period
  then raise_s [%message "bad clock" (period : int) (high : int)];
  let slew = Option.value slew ~default:t.config.input_slew in
  let g = { c_net = n; c_period = period; c_high = high; c_slew = slew; c_level = 0 } in
  (* level 0 from [now]; first rising edge at [start] *)
  ignore (push t ~time:t.now (Input (n, 0, slew)) : int);
  ignore (push t ~time:(Int.max start t.now) (Clock_toggle g) : int)
;;

let on_change t name f =
  let n = net_exn t name in
  t.watchers.(n) <- t.watchers.(n) @ [ (fun time v -> f ~time (Value.of_int v)) ]
;;

let metastable_events t = List.rev t.metastable

let instance_factors t =
  Array.to_list t.elements
  |> List.filter_map ~f:(function
    | Cell_e c -> Some (c.inst_name, c.factor)
    | _ -> None)
;;

(* --- VCD -------------------------------------------------------------------- *)

let start_tracing t = if Option.is_none t.trace then t.trace <- Some []

let vcd_id i =
  let rec go i acc =
    let acc = Char.of_int_exn (33 + (i % 94)) :: acc in
    if i < 94 then String.of_char_list acc else go ((i / 94) - 1) acc
  in
  go i []
;;

(** One name per net: the shortest, then alphabetically first. *)
let net_display_names t =
  let best = Array.create ~len:t.num_nets None in
  Hashtbl.iteri t.names ~f:(fun ~key ~data ->
    List.iter data ~f:(fun n ->
      match best.(n) with
      | None -> best.(n) <- Some key
      | Some b ->
        if String.length key < String.length b
           || (String.length key = String.length b && String.( < ) key b)
        then best.(n) <- Some key))
  ;
  best
;;

let write_vcd t oc =
  let trace = Option.value t.trace ~default:[] |> List.rev in
  let names = net_display_names t in
  let out = Stdio.Out_channel.output_string oc in
  out "$timescale 1fs $end\n$scope module event_sim $end\n";
  Array.iteri names ~f:(fun n name ->
    Option.iter name ~f:(fun name ->
      let safe = String.map name ~f:(fun c -> if Char.equal c ' ' then '_' else c) in
      out (Printf.sprintf "$var wire 1 %s %s $end\n" (vcd_id n) safe)));
  out "$upscope $end\n$enddefinitions $end\n#0\n$dumpvars\n";
  Array.iteri names ~f:(fun n name ->
    if Option.is_some name then out (Printf.sprintf "x%s\n" (vcd_id n)));
  out "$end\n";
  let last = ref (-1) in
  List.iter trace ~f:(fun (time, n, v) ->
    if Option.is_some names.(n)
    then (
      if time <> !last
      then (
        out (Printf.sprintf "#%d\n" time);
        last := time);
      out (Printf.sprintf "%c%s\n" (Value.to_char (Value.of_int v)) (vcd_id n))))
;;
