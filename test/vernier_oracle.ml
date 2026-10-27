(* Closed-form delay of a Vernier line from the raw Liberty tables
   (test/oracle.ml), independent of Timing_library and Event_sim.

   It walks the path an edge takes, from the clock pin of the launch flop to
   the [out] port, through the netlist the Vernier documentation describes:
   launch flop -> coarse chain (stages of [coarse_stage]) -> equal-depth mux2
   tree -> inverter -> coarse-bypass mux -> fine chain -> mux2 tree ->
   fine-bypass mux -> XOR with the other line -> output mux. Each net's load
   is the input capacitance of its sinks plus the Liberty wire-load model's
   capacitance at its fanout; slews propagate along the path. *)

open! Base

type t =
  { lib : Oracle.lib
  ; wire_model : string
  ; coarse_taps : int
  ; fine_taps : int
  ; coarse_stage : string list
  ; fine_stage : string list
  ; inverter : string
  ; mux2 : string
  ; xor2 : string
  ; flop : string
  ; clock_slew : float (* ns *)
  ; out_extra : (string * string) list (* cell, pin: loads on the out net besides the port *)
  ; xor_extra_fanout : int (* Hardcaml inputs on the XOR output (free, but counted) *)
  }

let pin_cap t (cell, pin) = Oracle.pin_cap t.lib ~cell ~pin
let wire t fanout = Oracle.wire_cap t.lib ~model:t.wire_model ~fanout
let load t sinks ~extra = List.sum (module Float) sinks ~f:(pin_cap t) +. wire t (List.length sinks + extra)

(* Single-input cell pins: the input is "A", the output "X" (buffers, delay
   cells) or "Y" (inverters), as in sg13cmos5l. *)
let out_pin cell = if String.is_substring cell ~substring:"inv" then "Y" else "X"
let inverting cell = String.is_substring cell ~substring:"inv"

(* Sinks of tap [k] of an equal-depth tree with [n] leaves: the pins of
   level-1 muxes it drives. *)
let leaf_pins t ~n k =
  let last_odd = n % 2 = 1 && k = n - 1 in
  if last_odd then [ t.mux2, "A0"; t.mux2, "A1" ] else [ t.mux2, if k % 2 = 0 then "A0" else "A1" ]
;;

(* The tree path for select [sel]: (input pin, load on the mux output) per
   level, root last. [root_sinks] load the root. *)
let tree_path t ~n ~sel ~root_sinks =
  let rec go level count index acc =
    if count = 1
    then List.rev acc
    else (
      let pin = if index % 2 = 0 then "A0" else "A1" in
      let parent_count = (count + 1) / 2 in
      let parent = index / 2 in
      let sinks =
        if parent_count = 1
        then root_sinks
        else leaf_pins t ~n:parent_count parent
      in
      ignore level;
      go (level + 1) parent_count parent ((pin, load t sinks ~extra:0) :: acc))
  in
  (* a select past the end aliases to the last input *)
  go 1 n (Int.min sel (n - 1)) []
;;

type state =
  { mutable time : float (* ns *)
  ; mutable slew : float
  ; mutable rise : bool (* direction of the net just reached *)
  }

let step t s ~cell ~input ~output ?when_ load =
  let arc = Oracle.timing t.lib ~cell ~out:output ~related:input ?when_ () in
  let d, sl = Oracle.arc arc ~rise:s.rise ~slew:s.slew ~load in
  s.time <- s.time +. d;
  s.slew <- sl
;;

(* Through a chain of [stages] stages of [stage] starting at a net whose
   own sinks are already accounted for; [tap_sinks k] are the extra sinks of
   tap [k] (after stage [k - 1]) besides the next stage. Returns at tap
   [upto]. *)
let chain t s ~stage ~stages ~upto ~tap_sinks =
  for k = 1 to upto do
    List.iteri stage ~f:(fun j cell ->
      let last = j = List.length stage - 1 in
      let sinks =
        if not last
        then [ List.nth_exn stage (j + 1), "A" ]
        else (if k < stages then [ List.hd_exn stage, "A" ] else []) @ tap_sinks k
      in
      if inverting cell then s.rise <- not s.rise;
      step t s ~cell ~input:"A" ~output:(out_pin cell) (load t sinks ~extra:0))
  done
;;

type mode =
  | Full
  | Coarse_only
  | Fine_only

(* Delay (ns) from the launch flop's clock edge to [out], for a launch flop
   Q going [q_rise], select [coarse]/[fine], [line] 0 (rise line, XOR pin A)
   or 1 (fall line, pin B), the other XOR input at [other]. *)
let line_delay t ~mode ~line ~q_rise ~other ~coarse ~fine =
  let s = { time = 0.; slew = t.clock_slew; rise = q_rise } in
  let uses_coarse, uses_fine =
    match mode with
    | Full -> true, true
    | Coarse_only -> true, false
    | Fine_only -> false, true
  in
  let cn = t.coarse_taps
  and fn = t.fine_taps in
  let first_stage stage = List.hd_exn stage, "A" in
  (* launch flop Q: first coarse stage, tree leaf 0, bypass inverter *)
  step
    t
    s
    ~cell:t.flop
    ~input:"CLK"
    ~output:"Q"
    (load t ((first_stage t.coarse_stage :: leaf_pins t ~n:cn 0) @ [ t.inverter, "A" ]) ~extra:0);
  if uses_coarse
  then (
    chain t s ~stage:t.coarse_stage ~stages:(cn - 1) ~upto:coarse ~tap_sinks:(leaf_pins t ~n:cn);
    let path = tree_path t ~n:cn ~sel:coarse ~root_sinks:[ t.inverter, "A" ] in
    List.iter path ~f:(fun (pin, l) -> step t s ~cell:t.mux2 ~input:pin ~output:"X" l);
    s.rise <- not s.rise;
    step t s ~cell:t.inverter ~input:"A" ~output:"Y" (load t [ t.mux2, "A0" ] ~extra:0))
  else (
    s.rise <- not s.rise;
    step t s ~cell:t.inverter ~input:"A" ~output:"Y" (load t [ t.mux2, "A1" ] ~extra:0));
  (* coarse-bypass mux, driving the fine chain input *)
  let fine_input_sinks = (first_stage t.fine_stage :: leaf_pins t ~n:fn 0) @ [ t.mux2, "A1" ] in
  step t s ~cell:t.mux2 ~input:(if uses_coarse then "A0" else "A1") ~output:"X" (load t fine_input_sinks ~extra:0);
  let xor_pin = if line = 0 then "A" else "B" in
  if uses_fine
  then (
    chain t s ~stage:t.fine_stage ~stages:(fn - 1) ~upto:fine ~tap_sinks:(leaf_pins t ~n:fn);
    let path = tree_path t ~n:fn ~sel:fine ~root_sinks:[ t.mux2, "A0" ] in
    List.iter path ~f:(fun (pin, l) -> step t s ~cell:t.mux2 ~input:pin ~output:"X" l));
  step t s ~cell:t.mux2 ~input:(if uses_fine then "A0" else "A1") ~output:"X" (load t [ t.xor2, xor_pin ] ~extra:0);
  let other_pin = if line = 0 then "B" else "A" in
  let when_ = if other then other_pin else "!" ^ other_pin in
  if other then s.rise <- not s.rise;
  step t s ~cell:t.xor2 ~input:xor_pin ~output:"X" ~when_ (load t [ t.mux2, "A0" ] ~extra:t.xor_extra_fanout);
  step t s ~cell:t.mux2 ~input:"A0" ~output:"X" (load t t.out_extra ~extra:0);
  s.time, s.rise
;;

(* Bypass: the level flop through the output mux. *)
let bypass_delay t ~q_rise =
  let s = { time = 0.; slew = t.clock_slew; rise = q_rise } in
  step t s ~cell:t.flop ~input:"CLK" ~output:"Q" (load t [ t.mux2, "A1" ] ~extra:0);
  step t s ~cell:t.mux2 ~input:"A1" ~output:"X" (load t t.out_extra ~extra:0);
  s.time
;;
