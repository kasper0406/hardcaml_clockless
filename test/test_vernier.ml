(* Vernier in Cyclesim: the fraction -> (coarse, fine) mapping against an
   independent integer reference, the control (latency, safe select
   updates, rate violations, mode switches) on the behavioural Model, the
   netlist structure and the Sdc constraints. Gate-level timing is in
   test_vernier_timing.ml. *)

open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

module V = Vernier.Make (struct
    let pins = 2
    let coarse_taps = 40
    let fine_taps = 16
    let fraction_bits = 8
  end)

(* The mapping as specified in vernier.mli, written out with integers. *)
let reference ~a ~scale ~offset ~ratio ~(mode : Vernier.Mode.t) =
  match mode with
  | Fine -> 0, a % 16, false
  | Bypass | Coarse | Full ->
    if scale = 0
    then (
      (* identity: the fraction is the coarse tap; past the last tap clamps
         and is a range fault in coarse and full mode *)
      let over = a > 39 in
      Int.min a 39, 0, over && ([%equal: Vernier.Mode.t] mode Coarse || [%equal: Vernier.Mode.t] mode Full))
    else (
      let p = (((a * scale) + 32) / 64) + offset in
      let c = p / 64 in
      let f = (((p % 64) * (16 + ratio)) + 64) / 128 in
      let c_over = c > 39
      and f_over = f > 15 in
      let coarse = if c_over then 39 else c in
      match mode with
      | Coarse -> coarse, 0, c_over
      | Full -> coarse, (if c_over || f_over then 15 else f), c_over || f_over
      | Bypass | Fine -> coarse, (if c_over || f_over then 15 else f), false)
;;

let%expect_test "mapping: hardware equals the integer reference" =
  let module C = Cyclesim.With_interface (V.Map.I) (V.Map.O) in
  let sim = C.create (fun i -> V.Map.create i) in
  let i = Cyclesim.inputs sim in
  let set (p : Bits.t ref) w v = p := Bits.of_int_trunc ~width:w v in
  let rng = Random.State.make [| 1 |] in
  let mismatches = ref 0 in
  let n = 50_000 in
  for k = 1 to n do
    let a = Random.State.int rng 256 in
    let scale =
      match k % 8 with
      | 0 -> 0
      | 1 | 2 -> Random.State.int rng 1024
      | _ -> 200 + Random.State.int rng 500
    in
    let offset = Random.State.int rng 256 in
    let ratio = Random.State.int rng 16 in
    let mode = List.nth_exn Vernier.Mode.all (Random.State.int rng 4) in
    set i.V.Map.I.fraction 8 a;
    set i.scale V.scale_bits scale;
    set i.offset Vernier.offset_bits offset;
    set i.ratio Vernier.ratio_bits ratio;
    set i.mode 2 (Vernier.Mode.to_int mode);
    Cyclesim.cycle sim;
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    let got = Bits.to_int_trunc !(o.V.Map.O.coarse), Bits.to_int_trunc !(o.fine), Bits.to_bool !(o.range_fault) in
    if not ([%equal: int * int * bool] got (reference ~a ~scale ~offset ~ratio ~mode)) then Int.incr mismatches
  done;
  printf "%d random inputs, mismatches %d\n" n !mismatches;
  [%expect {| 50000 random inputs, mismatches 0 |}]
;;

(* --- Control, on the behavioural model --- *)

(* 4096 ticks per cycle; at 50 MHz: 1.83 ns base, 830 ps coarse, 85 ps fine. *)
let model : Vernier.model = { base_ticks = 375; coarse_ticks = 170; fine_ticks = 17 }

module Sim = struct
  type t =
    { sim : Cyclesim.With_interface(V.I)(V.O).t
    ; i : Bits.t ref V.I.t
    ; o : Bits.t ref V.O.t
    ; mutable cycle : int
    ; toggles : (int * int * int) Queue.t (* cycle, line, ticks *)
    }

  let create () =
    let module C = Cyclesim.With_interface (V.I) (V.O) in
    let scope = Scope.create ~flatten_design:true () in
    let sim = C.create (V.create (Model model) scope) in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    let t = { sim; i; o; cycle = 0; toggles = Queue.create () } in
    let set p w v = p := Bits.of_int_trunc ~width:w v in
    set i.settle_cycles Vernier.settle_bits 4;
    (* the model's ideal constants: 4096 / 170 coarse steps per cycle, R = 10 *)
    List.iter i.scale ~f:(fun p -> set p V.scale_bits (4096 * 16 / 170));
    List.iter i.offset ~f:(fun p -> set p Vernier.offset_bits 0);
    List.iter i.ratio ~f:(fun p -> set p Vernier.ratio_bits 4);
    i.clear := Bits.vdd;
    t
  ;;

  let set p w v = p := Bits.of_int_trunc ~width:w v

  let step t =
    Cyclesim.cycle t.sim;
    t.cycle <- t.cycle + 1;
    List.iteri t.o.model_toggle ~f:(fun l x ->
      if Bits.to_bool !x then Queue.enqueue t.toggles (t.cycle, l, Bits.to_int_trunc !(List.nth_exn t.o.model_ticks l)))
  ;;

  let run t n =
    for _ = 1 to n do
      step t
    done
  ;;

  let get l p = Bits.to_int_trunc !(List.nth_exn l p)
end

let%expect_test "control: latency, safe select updates, overruns, mode switches" =
  let open Sim in
  let t = create () in
  run t 2;
  t.i.clear := Bits.gnd;
  run t 2;
  printf "after clear: mode %d\n" (get t.o.active_mode 0);
  let mode m = set (List.hd_exn t.i.mode) 2 (Vernier.Mode.to_int m) in
  let identity () = List.iter t.i.scale ~f:(fun p -> set p V.scale_bits 0) in
  identity ();
  mode Full;
  run t 8;
  let edge ~level ~fraction =
    set (List.hd_exn t.i.fraction) 8 fraction;
    set (List.hd_exn t.i.level) 1 level;
    let e = t.cycle + 1 in
    step t;
    e
  in
  let report what e =
    let ts = Queue.to_list t.toggles in
    Queue.clear t.toggles;
    printf
      "%s (registered at edge %d): %s | overrun %d, fallback %d, out %d\n"
      what e
      (String.concat
         ~sep:", "
         (List.map ts ~f:(fun (c, l, k) -> Printf.sprintf "line %d toggles in cycle +%d at %d ticks" l (c - e) k)))
      (get t.o.overrun 0) (get t.o.fallback 0) (get t.o.out 0)
  in
  (* identity, full mode: coarse tap 3: 375 + 3 * 170 = 885 ticks; launch at edge e + 2 *)
  let e = edge ~level:1 ~fraction:3 in
  run t 8;
  report "rise, coarse tap 3" e;
  (* tap 39: 375 + 6630 = 7005 ticks = 1 cycle + 2909 *)
  let e = edge ~level:0 ~fraction:39 in
  run t 8;
  report "fall, coarse tap 39" e;
  (* the rise line again two cycles after its last edge, with a new tap *)
  let e = edge ~level:1 ~fraction:20 in
  ignore (edge ~level:0 ~fraction:20 : int);
  ignore (edge ~level:1 ~fraction:0 : int);
  run t 8;
  report "rise 20, fall 20, rise 0 (one cycle apart)" e;
  (* acknowledge and re-arm *)
  set (List.hd_exn t.i.clear_flags) 1 1;
  set (List.hd_exn t.i.mode_write) 1 1;
  step t;
  set (List.hd_exn t.i.clear_flags) 1 0;
  set (List.hd_exn t.i.mode_write) 1 0;
  (* an unchanged tap on a busy line: allowed *)
  let e = edge ~level:0 ~fraction:0 in
  ignore (edge ~level:1 ~fraction:0 : int);
  ignore (edge ~level:0 ~fraction:0 : int);
  ignore (edge ~level:1 ~fraction:0 : int);
  run t 8;
  report "edges every cycle with an unchanged tap" e;
  (* a mode change waits for both lines to be quiet *)
  let e = edge ~level:0 ~fraction:39 in
  mode Fine;
  let changed = ref None in
  for _ = 1 to 12 do
    step t;
    if Option.is_none !changed && get t.o.active_mode 0 = Vernier.Mode.to_int Fine
    then changed := Some (t.cycle - e)
  done;
  Queue.clear t.toggles;
  printf "mode change requested after an edge registered at %d: applied %d edges later (launch at +2, settled at settle 4 + 2)\n"
    e (Option.value_exn !changed);
  (* fine mode: the fraction's low bits are the fine tap: 375 + 7 * 17 = 494 *)
  let e = edge ~level:1 ~fraction:(16 + 7) in
  run t 8;
  report "fine mode, fraction 23" e;
  (* a range fault: 64 coarse steps per cycle, a line of 40 taps (as at a
     clock slower than the line) *)
  mode Full;
  List.iter t.i.scale ~f:(fun p -> set p V.scale_bits 1023);
  run t 8;
  let e = edge ~level:0 ~fraction:255 in
  run t 8;
  printf "range fault (registered at edge %d): range_fault %d, fallback %d, active mode %d\n" e (get t.o.range_fault 0)
    (get t.o.fallback 0) (get t.o.active_mode 0);
  Queue.clear t.toggles;
  [%expect {|
    after clear: mode 0
    rise, coarse tap 3 (registered at edge 13): line 0 toggles in cycle +2 at 885 ticks | overrun 0, fallback 0, out 1
    fall, coarse tap 39 (registered at edge 22): line 1 toggles in cycle +3 at 2909 ticks | overrun 0, fallback 0, out 0
    rise 20, fall 20, rise 0 (one cycle apart) (registered at edge 31): line 0 toggles in cycle +2 at 3775 ticks, line 1 toggles in cycle +3 at 3775 ticks, line 0 toggles in cycle +4 at 3775 ticks | overrun 1, fallback 1, out 1
    edges every cycle with an unchanged tap (registered at edge 43): line 1 toggles in cycle +2 at 375 ticks, line 0 toggles in cycle +3 at 375 ticks, line 1 toggles in cycle +4 at 375 ticks, line 0 toggles in cycle +5 at 375 ticks | overrun 0, fallback 0, out 1
    mode change requested after an edge registered at 55: applied 8 edges later (launch at +2, settled at settle 4 + 2)
    fine mode, fraction 23 (registered at edge 68): line 0 toggles in cycle +2 at 494 ticks | overrun 0, fallback 0, out 1
    range fault (registered at edge 85): range_fault 1, fallback 1, active mode 3
    |}]
;;

let%expect_test "control: lines count as busy after a clear; identity range faults" =
  let open Sim in
  (* A launch flop reset from 1 sends an edge into its line, so a mode
     written right after a clear must wait settle_cycles like any other. *)
  let t = create () in
  run t 2;
  set (List.hd_exn t.i.mode) 2 (Vernier.Mode.to_int Full);
  t.i.clear := Bits.gnd;
  let applied = ref None in
  for k = 1 to 12 do
    step t;
    if Option.is_none !applied && get t.o.active_mode 0 = Vernier.Mode.to_int Full then applied := Some k
  done;
  printf "Full requested at the clear's release: applied after %d cycles (settled at settle 4 + 2)\n" (Option.value_exn !applied);
  (* identity scale: the fraction is the coarse tap; past the last (39) is a
     range fault, not a silent clamp *)
  List.iter t.i.scale ~f:(fun p -> set p V.scale_bits 0);
  List.iter [ 39; 45 ] ~f:(fun fraction ->
    set (List.hd_exn t.i.clear_flags) 1 1;
    set (List.hd_exn t.i.mode_write) 1 1;
    run t 8;
    set (List.hd_exn t.i.clear_flags) 1 0;
    set (List.hd_exn t.i.mode_write) 1 0;
    run t 8;
    set (List.hd_exn t.i.fraction) 8 fraction;
    set (List.hd_exn t.i.level) 1 (1 - get t.o.out 0);
    run t 8;
    printf "identity, full mode, fraction %d: range_fault %d, fallback %d\n" fraction (get t.o.range_fault 0)
      (get t.o.fallback 0));
  Queue.clear t.toggles;
  [%expect {|
    Full requested at the clear's release: applied after 6 cycles (settled at settle 4 + 2)
    identity, full mode, fraction 39: range_fault 0, fallback 0
    identity, full mode, fraction 45: range_fault 1, fallback 1
    |}]
;;

let%expect_test "control: mapped fractions on the ideal model land at a / 256 of a cycle" =
  let open Sim in
  let t = create () in
  run t 2;
  t.i.clear := Bits.gnd;
  set (List.hd_exn t.i.mode) 2 (Vernier.Mode.to_int Full);
  (* offset 0: the model's base delay is d_align *)
  run t 10;
  let worst = ref 0 in
  for a = 0 to 255 do
    set (List.hd_exn t.i.fraction) 8 a;
    set (List.hd_exn t.i.level) 1 (1 - get t.i.level 0);
    let launch = t.cycle + 2 in
    run t 8;
    match Queue.to_list t.toggles with
    | [ (c, _, k) ] ->
      Queue.clear t.toggles;
      let ticks = ((c - launch - 1) * 4096) + k in
      let want = model.base_ticks + (a * 16) in
      worst := Int.max !worst (abs (ticks - want))
    | _ -> failwith "expected one toggle"
  done;
  printf "worst error %d ticks (fine step %d)\n" !worst model.fine_ticks;
  [%expect {| worst error 13 ticks (fine step 17) |}]
;;

let%expect_test "control: clearing the flags acknowledges a fault; only a mode write re-arms" =
  let open Sim in
  let t = create () in
  run t 2;
  t.i.clear := Bits.gnd;
  set (List.hd_exn t.i.mode) 2 (Vernier.Mode.to_int Full);
  run t 10;
  let edge ~level ~fraction =
    set (List.hd_exn t.i.fraction) 8 fraction;
    set (List.hd_exn t.i.level) 1 level;
    step t
  in
  let pulse l =
    set (List.hd_exn l) 1 1;
    step t;
    set (List.hd_exn l) 1 0
  in
  let show what =
    printf "%-44s overrun %d, fallback %d, active mode %d\n" what (get t.o.overrun 0) (get t.o.fallback 0)
      (get t.o.active_mode 0)
  in
  (* Cycles from a request until [out] shows it. Fraction 250 is 4370 ticks
     of line delay, past the end of the launch cycle: one cycle more than
     bypass. *)
  let latencies n =
    let seen = ref [] in
    for _ = 1 to n do
      let level = 1 - get t.i.level 0 in
      edge ~level ~fraction:250;
      let c = ref 1 in
      while get t.o.out 0 <> level && !c < 10 do
        step t;
        Int.incr c
      done;
      run t (10 - !c);
      seen := !c :: !seen
    done;
    List.dedup_and_sort !seen ~compare
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:","
  in
  let overrun () =
    edge ~level:1 ~fraction:20;
    edge ~level:0 ~fraction:20;
    edge ~level:1 ~fraction:0;
    edge ~level:0 ~fraction:0;
    run t 10
  in
  printf "armed: request -> out latency %s cycles\n" (latencies 4);
  overrun ();
  show "overrun";
  pulse t.i.clear_flags;
  run t 10;
  show "clear_flags";
  printf "request -> out latency %s cycles (bypass)\n" (latencies 8);
  show "after 8 spaced edges";
  pulse t.i.mode_write;
  run t 10;
  show "mode_write (unchanged mode)";
  printf "request -> out latency %s cycles\n" (latencies 4);
  overrun ();
  show "overrun";
  (* a mode write while an edge is in flight waits for the lines to be quiet *)
  edge ~level:1 ~fraction:250;
  pulse t.i.mode_write;
  let rearmed = ref 0 in
  while get t.o.fallback 0 = 1 && !rearmed < 50 do
    step t;
    Int.incr rearmed
  done;
  printf "mode_write one cycle after a request: re-armed %d cycles later (settled at settle 4 + 2)\n" !rearmed;
  show "mode_write without clear_flags";
  (* a fault discards a pending re-arm *)
  overrun ();
  pulse t.i.clear_flags;
  edge ~level:1 ~fraction:30;
  pulse t.i.mode_write;
  edge ~level:0 ~fraction:30;
  edge ~level:1 ~fraction:60;
  run t 20;
  show "mode_write, then an overrun before it applied";
  printf "pin 1 untouched: overrun %d, fallback %d\n" (get t.o.overrun 1) (get t.o.fallback 1);
  Queue.clear t.toggles;
  [%expect {|
    armed: request -> out latency 4 cycles
    overrun                                      overrun 1, fallback 1, active mode 3
    clear_flags                                  overrun 0, fallback 1, active mode 3
    request -> out latency 3 cycles (bypass)
    after 8 spaced edges                         overrun 0, fallback 1, active mode 3
    mode_write (unchanged mode)                  overrun 0, fallback 0, active mode 3
    request -> out latency 4 cycles
    overrun                                      overrun 1, fallback 1, active mode 3
    mode_write one cycle after a request: re-armed 7 cycles later (settled at settle 4 + 2)
    mode_write without clear_flags               overrun 1, fallback 0, active mode 3
    mode_write, then an overrun before it applied overrun 1, fallback 1, active mode 3
    pin 1 untouched: overrun 0, fallback 0
    |}]
;;

let%expect_test "control: the edge-rate limit is B = settle_cycles + 1 cycles per line" =
  let open Sim in
  List.iter [ 1; 3; 4; 7 ] ~f:(fun settle ->
    let t = create () in
    set t.i.settle_cycles Vernier.settle_bits settle;
    run t 2;
    t.i.clear := Bits.gnd;
    set (List.hd_exn t.i.mode) 2 (Vernier.Mode.to_int Full);
    run t 20;
    let request ~level ~fraction =
      set (List.hd_exn t.i.fraction) 8 fraction;
      set (List.hd_exn t.i.level) 1 level;
      step t
    in
    (* rise (fraction 10), fall a cycle later, rise with a new select [gap]
       cycles after the first *)
    let overruns gap =
      set (List.hd_exn t.i.clear_flags) 1 1;
      set (List.hd_exn t.i.mode_write) 1 1;
      step t;
      set (List.hd_exn t.i.clear_flags) 1 0;
      set (List.hd_exn t.i.mode_write) 1 0;
      run t 20;
      request ~level:1 ~fraction:10;
      request ~level:0 ~fraction:10;
      run t (gap - 2);
      request ~level:1 ~fraction:100;
      run t 20;
      let o = get t.o.overrun 0 in
      request ~level:0 ~fraction:100;
      run t 20;
      o
    in
    printf "settle_cycles %d: overrun at gap %s\n" settle
      (String.concat ~sep:" "
         (List.map (List.range 2 10) ~f:(fun gap -> Printf.sprintf "%d:%d" gap (overruns gap)))));
  [%expect {|
    settle_cycles 1: overrun at gap 2:0 3:0 4:0 5:0 6:0 7:0 8:0 9:0
    settle_cycles 3: overrun at gap 2:1 3:1 4:0 5:0 6:0 7:0 8:0 9:0
    settle_cycles 4: overrun at gap 2:1 3:1 4:1 5:0 6:0 7:0 8:0 9:0
    settle_cycles 7: overrun at gap 2:1 3:1 4:1 5:1 6:1 7:1 8:0 9:0
    |}]
;;

let%expect_test "calibration: constants that do not fit their fields are refused, not clipped" =
  let line ?(zero_ps = 2000.) ?(fine_ps = 100.) ~coarse_ps () : Vernier.Calibration.line =
    let sweep : Vernier.Calibration.sweep =
      { coarse = Array.init 40 ~f:(fun k -> zero_ps +. (Float.of_int k *. coarse_ps))
      ; fine = Array.init 16 ~f:(fun k -> zero_ps +. (Float.of_int k *. fine_ps))
      }
    in
    { rising = sweep; falling = sweep }
  in
  let try_ what ?(mode = Vernier.Mode.Full) ~period_ps lines =
    match Vernier.Calibration.fit ~coarse_taps:40 ~period_ps ~mode (Array.of_list lines) with
    | c ->
      printf "%s: scale %s offset %s ratio %s\n" what
        (Sexp.to_string [%sexp (c.scale : int array)])
        (Sexp.to_string [%sexp (c.offset : int array)])
        (Sexp.to_string [%sexp (c.ratio : int array)])
    | exception e -> printf "%s: %s\n" what (Sexp.to_string (Exn.sexp_of_t e))
  in
  let ns1 = line ~coarse_ps:1000. () in
  try_ "50 MHz, 1 ns steps" ~period_ps:20_000. [ ns1; ns1 ];
  (* 100 ns / 1 ns = 100 taps per cycle: scale 1600; clipped to 1023,
     fraction 64 would come out at 16 ns instead of 25, with no fault *)
  try_ "10 MHz, 1 ns steps" ~period_ps:100_000. [ ns1 ];
  try_ "63.94 steps per cycle" ~period_ps:63_937.5 [ ns1 ];
  try_ "64 steps per cycle" ~period_ps:64_000. [ ns1 ];
  try_ "zero delays 5 steps apart" ~period_ps:20_000. [ ns1; line ~zero_ps:7000. ~coarse_ps:1000. () ];
  try_ "R = 20" ~period_ps:20_000. [ line ~coarse_ps:1000. ~fine_ps:50. () ];
  try_ "R = 20, coarse mode (ratio unused)" ~mode:Coarse ~period_ps:20_000. [ line ~coarse_ps:1000. ~fine_ps:50. () ];
  try_ "R = 6.7" ~period_ps:20_000. [ line ~coarse_ps:1000. ~fine_ps:150. () ];
  try_ "coarse step 1e-300 ps" ~period_ps:20_000. [ line ~coarse_ps:1e-300 () ];
  try_ "NaN fine sweep" ~period_ps:20_000. [ line ~coarse_ps:1000. ~fine_ps:Float.nan () ];
  try_ "NaN period" ~period_ps:Float.nan [ ns1 ];
  [%expect {|
    50 MHz, 1 ns steps: scale (320 320) offset (0 0) ratio (4 4)
    10 MHz, 1 ns steps: ("Vernier.Calibration.fit: constant out of range"(what scale)(line 0)(x 1600)(lo 1)(hi 1023))
    63.94 steps per cycle: scale (1023) offset (0) ratio (4)
    64 steps per cycle: ("Vernier.Calibration.fit: constant out of range"(what scale)(line 0)(x 1024)(lo 1)(hi 1023))
    zero delays 5 steps apart: ("Vernier.Calibration.fit: constant out of range"(what offset)(line 0)(x 320)(lo 0)(hi 255))
    R = 20: ("Vernier.Calibration.fit: constant out of range"(what ratio)(line 0)(x 24)(lo 0)(hi 15))
    R = 20, coarse mode (ratio unused): scale (320) offset (32) ratio (0)
    R = 6.7: ("Vernier.Calibration.fit: constant out of range"(what ratio)(line 0)(x -2.6666666666666661)(lo 0)(hi 15))
    coarse step 1e-300 ps: ("Vernier.Calibration.fit: constant out of range"(what scale)(line 0)(x INF)(lo 1)(hi 1023))
    NaN fine sweep: ("Vernier.Calibration.fit: constant out of range"(what ratio)(line 0)(x NAN)(lo 0)(hi 15))
    NaN period: ("Vernier.Calibration.fit: bad period"(period_ps NAN))
    |}]
;;

(* --- Structure and constraints --- *)

let lib = lazy (Timing_library.load "data/sg13cmos5l_excerpt.lib")

let cells () : Vernier.cells =
  { library = (force lib).library
  ; coarse_stage = [ "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1"; "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1" ]
  ; fine_stage = [ "sg13cmos5l_buf_1" ]
  ; inverter = "sg13cmos5l_inv_1"
  ; mux2 = "sg13cmos5l_mux2_1"
  ; xor2 = "sg13cmos5l_xor2_1"
  ; buffer = "sg13cmos5l_buf_1"
  ; flop = { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
  }
;;

let circuit () =
  let module C = Circuit.With_interface (V.I) (V.O) in
  C.create_exn ~name:"vernier_top" (V.create (Cells (cells ())) (Scope.create ~flatten_design:true ()))
;;

let%expect_test "netlist: hand-placed cells per pin, stable names" =
  let sdc = Sdc.create (circuit ()) in
  let by_cell = Hashtbl.create (module String) in
  List.iter (Sdc.instances sdc) ~f:(fun i -> Hashtbl.incr by_cell i.cell);
  Hashtbl.to_alist by_cell
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.iter ~f:(fun (c, n) -> printf "%-26s %4d\n" c n);
  let names = List.map (Sdc.instances sdc) ~f:(fun i -> i.name) in
  List.iter
    [ "vernier_p0_r_launch"; "vernier_p0_r_c_stage0_0"; "vernier_p0_r_c_stage38_3"; "vernier_p0_r_cm6_0"
    ; "vernier_p0_r_cinv"; "vernier_p0_r_cbinv"; "vernier_p0_r_cbyp"; "vernier_p0_r_f_stage14_0"
    ; "vernier_p0_r_fm4_0"; "vernier_p0_r_fbyp"; "vernier_p1_f_fbyp"; "vernier_p0_xor"; "vernier_p0_level"
    ; "vernier_p0_out" ]
    ~f:(fun n -> if not (List.mem names n ~equal:String.equal) then printf "missing %s\n" n);
  [%expect {|
    sg13cmos5l_buf_1             80
    sg13cmos5l_dfrbpq_1           6
    sg13cmos5l_dlygate4sd3_1    312
    sg13cmos5l_inv_1            320
    sg13cmos5l_mux2_1           234
    sg13cmos5l_xor2_1             2
    |}]
;;

let%expect_test "Sdc.vernier: lines cut from registers and ports, select and drain bounds; dont-touch covers the lines" =
  let sdc = Sdc.create (circuit ()) |> Sdc.vernier ~name:"vernier" ~pins:2 ~period_ns:20. ~settle_cycles:4 in
  (* the pin lists are long: show each command's head and what it lists *)
  String.split_lines (Sdc.sdc sdc ~stage:Pnr ~system_clock:"clk")
  |> List.iter ~f:(fun l ->
    match String.lsplit2 l ~on:'{' with
    | Some (head, _) when String.is_prefix l ~prefix:"set_max_delay" && String.length l > 160 ->
      let pins = String.split (String.drop_suffix (String.rsplit2_exn l ~on:'{' |> snd) 2) ~on:' ' in
      let by_pin =
        List.map pins ~f:(fun p -> String.rsplit2_exn p ~on:'/' |> snd)
        |> List.sort_and_group ~compare:String.compare
        |> List.map ~f:(fun g -> Printf.sprintf "%d %s" (List.length g) (List.hd_exn g))
      in
      printf "%s... (%s pins, first %s)\n" head (String.concat ~sep:" " by_pin) (List.hd_exn pins)
    | _ -> print_endline l);
  let nets = Sdc.net_names sdc in
  printf "protected nets: %d, e.g. %s\n" (List.length nets)
    (String.concat ~sep:" " (List.filter nets ~f:(fun n -> String.is_prefix n ~prefix:"vernier_p0_r_cm6")));
  (* the select pins are the muxes' S, never a data input *)
  List.iter (Sdc.max_delays sdc) ~f:(fun d ->
    if not (List.is_empty d.from)
    then
      printf "drain: from %s to %s (driven by the XOR)\n" (String.concat ~sep:" " d.from)
        (String.concat ~sep:" " (List.map d.to_ ~f:Sdc.Endpoint.to_string)));
  let line_muxes =
    List.filter_map (Sdc.instances sdc) ~f:(fun i ->
      Option.some_if (String.equal i.cell "sg13cmos5l_mux2_1" && not (String.is_suffix i.name ~suffix:"_out")) (i.name ^ "/S"))
  in
  let selects =
    List.concat_map (Sdc.max_delays sdc) ~f:(fun d -> d.to_)
    |> List.filter_map ~f:(fun e -> Option.some_if (String.equal e.pin "S") (Sdc.Endpoint.to_string e))
  in
  printf "select bound covers exactly the S pin of every mux but the output muxes (%d): %b\n"
    (List.length line_muxes)
    ([%equal: string list] (List.sort line_muxes ~compare:String.compare) (List.sort selects ~compare:String.compare));
  [%expect {|
    # hardcaml_clockless islands (PnR): append after the system clock is created.
    set_false_path -from [get_cells {vernier_p0_r_launch vernier_p0_f_launch}] -to [all_registers]
    set_false_path -from [get_cells {vernier_p0_r_launch vernier_p0_f_launch}] -to [all_outputs]
    set_false_path -from [get_cells {vernier_p1_r_launch vernier_p1_f_launch}] -to [all_registers]
    set_false_path -from [get_cells {vernier_p1_r_launch vernier_p1_f_launch}] -to [all_outputs]
    set_max_delay 20.0000 -to [get_pins ... (58 S pins, first vernier_p0_r_cm1_0/S)
    set_max_delay 20.0000 -to [get_pins ... (58 S pins, first vernier_p0_f_cm1_0/S)
    set_max_delay 80.0000 -from [get_cells {vernier_p0_r_launch vernier_p0_f_launch}] -to [get_pins {vernier_p0_out/A0}]
    set_max_delay 20.0000 -to [get_pins ... (58 S pins, first vernier_p1_r_cm1_0/S)
    set_max_delay 20.0000 -to [get_pins ... (58 S pins, first vernier_p1_f_cm1_0/S)
    set_max_delay 80.0000 -from [get_cells {vernier_p1_r_launch vernier_p1_f_launch}] -to [get_pins {vernier_p1_out/A0}]
    protected nets: 1013, e.g. vernier_p0_r_cm6_0__X
    drain: from vernier_p0_r_launch vernier_p0_f_launch to vernier_p0_out/A0 (driven by the XOR)
    drain: from vernier_p1_r_launch vernier_p1_f_launch to vernier_p1_out/A0 (driven by the XOR)
    select bound covers exactly the S pin of every mux but the output muxes (232): true
    |}]
;;

let%expect_test "Sdc.vernier: argument checks" =
  let try_ f = match f () with _ -> print_endline "ok" | exception e -> print_s (Exn.sexp_of_t e) in
  let sdc = Sdc.create (circuit ()) in
  try_ (fun () -> Sdc.vernier sdc ~name:"vernier" ~pins:2 ~period_ns:20. ~settle_cycles:0);
  try_ (fun () -> Sdc.vernier sdc ~name:"vernier" ~pins:2 ~period_ns:20. ~settle_cycles:8);
  try_ (fun () -> Sdc.vernier sdc ~name:"vernier" ~pins:2 ~period_ns:0. ~settle_cycles:3);
  try_ (fun () -> Sdc.vernier sdc ~name:"vernier" ~pins:3 ~period_ns:20. ~settle_cycles:3);
  [%expect {|
    ("Sdc.vernier: settle_cycles out of range" (settle_cycles 0))
    ("Sdc.vernier: settle_cycles out of range" (settle_cycles 8))
    ("Sdc.vernier: bad period" (period_ns 0))
    ("Sdc: no protected instance" (instance vernier_p2_r_launch))
    |}]
;;

let%expect_test "configuration errors" =
  let try_ f = match f () with _ -> print_endline "ok" | exception e -> print_s (Exn.sexp_of_t e) in
  try_ (fun () ->
    let module _ = Vernier.Make (struct let pins = 2 let coarse_taps = 1 let fine_taps = 16 let fraction_bits = 8 end) in
    ());
  try_ (fun () ->
    let module _ = Vernier.Make (struct let pins = 2 let coarse_taps = 40 let fine_taps = 16 let fraction_bits = 2 end) in
    ());
  try_ (fun () ->
    let module C = Circuit.With_interface (V.I) (V.O) in
    C.create_exn ~name:"x" (V.hierarchical ~module_name:"v" (Model model) (Scope.create ~flatten_design:false ())));
  try_ (fun () ->
    Delay_line.create_stages (force lib).library ~name:"d" ~stage:[ "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1" ] ~length:2 Signal.gnd);
  [%expect {|
    ("Vernier: bad configuration" (msg "coarse_taps must be 2..64"))
    ("Vernier: bad configuration" (msg "fraction_bits must be 3..12"))
    ("Vernier.Model is simulation-only: use it in a scope with ~flatten_design:true"
     (module_name v))
    ("delay line stage inverts"
     (stage (sg13cmos5l_dlygate4sd3_1 sg13cmos5l_inv_1)))
    |}]
;;

let%expect_test "no protected net drives more than Cell.max_fanout cells; controls isolated" =
  let sdc = Sdc.create (circuit ()) in
  let worst =
    List.fold (Sdc.protected_nets sdc) ~init:("", 0) ~f:(fun (n, m) (net : Sdc.Net.t) ->
      let k = List.length net.sinks in
      if k > m then String.concat ~sep:"," net.names, k else n, m)
  in
  print_s [%sexp (worst : string * int)];
  (* every select and the reset come from synthesised logic through an
     isolating buffer, so the nets into the cells are boundary nets of their
     own; the rest are the launch and level flops' D, one load each *)
  let boundary = List.filter (Sdc.protected_nets sdc) ~f:(fun n -> n.boundary) in
  let iso, other =
    List.partition_tf boundary ~f:(fun n ->
      List.exists n.names ~f:(fun nm -> String.is_suffix nm ~suffix:"_iso__X"))
  in
  printf "boundary nets: %d from isolating buffers, %d others\n" (List.length iso) (List.length other);
  List.iter other ~f:(fun n ->
    print_s [%message "" ~sinks:(List.map n.sinks ~f:Sdc.Endpoint.to_string : string list)]);
  [%expect {|
    (vernier_p0_r_sel_fm1_iso__X 8)
    boundary nets: 51 from isolating buffers, 6 others
    (sinks (vernier_p0_r_launch/D))
    (sinks (vernier_p0_f_launch/D))
    (sinks (vernier_p0_level/D))
    (sinks (vernier_p1_r_launch/D))
    (sinks (vernier_p1_f_launch/D))
    (sinks (vernier_p1_level/D))
    |}]
;;
