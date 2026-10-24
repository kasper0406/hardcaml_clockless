(* Ring under Event_sim with Liberty delays: the real Ring module (flop-cell
   counters and sampler) at three corners, with and without the length-select
   mux in the loop.

   Findings recorded by these tests:
   - per-cycle decoded time is exact on the median, with no pattern errors
     or X, despite ~1.5 metastable sampler captures per cycle;
   - counters settle 6.4-9.5 stages after their edge, inside the selection
     threshold;
   - stages are not uniform, so uncalibrated per-cycle deltas spread by more
     than 2 stages; code-density calibration fixes that;
   - the length-select mux in the loop creates bins up to ~400 ps and doubles
     the calibrated error. A fixed-length ring (bins 16-114 ps across corners,
     27-72 ps rms error on 20 ns intervals) is the better default; escape
     injection locking by changing the clock frequency instead. *)

open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

let corners =
  [ "typ", "data/sg13cmos5l_excerpt.lib"
  ; "slow", "data/sg13cmos5l_excerpt_slow_1p08V_125C.lib"
  ; "fast", "data/sg13cmos5l_excerpt_fast_1p32V_m40C.lib"
  ]
;;

let dfrbpq_cell = "sg13cmos5l_dfrbpq_1"
let clock_period = Event_sim.of_ns 20.

module Measure (Config : Ring.Config) = struct
  module R = Ring.Make (Config)

  let dfrbpq : R.flop =
    { cell = dfrbpq_cell; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
  ;;

  let circuit (library : Cell_library.t) =
    let enable = Signal.input "enable" 1 in
    let length_sel = Signal.input "length_sel" R.select_width in
    let clock = Signal.input "clock" 1 in
    let state, _ =
      R.Analog.create
        ~name:"ring"
        (Cells
           { library
           ; nand2 = "sg13cmos5l_nand2_1"
           ; inverter = "sg13cmos5l_inv_1"
           ; mux2 = "sg13cmos5l_mux2_1"
           ; flop = Some dfrbpq
           })
        ~enable
        ~length_sel
    in
    let scope = Scope.create ~flatten_design:true () in
    let o =
      R.Clk_sampler.create
        ~first_stage:(library, dfrbpq)
        ~name:"ring"
        scope
        { clock; clear = Signal.gnd; enable; length_sel; state }
    in
    Circuit.create_exn
      ~name:"ring_timing"
      (Signal.output "node0" (Signal.bit state.nodes ~pos:0)
       :: Signal.output "count_a" state.count_a
       :: Signal.output "count_b" state.count_b
       :: R.Clk_sampler.O.(to_list (map2 port_names o ~f:Signal.output)))
  ;;


  let cycles = 400

  let measure ~corner ~path ~n =
    let tl = Timing_library.load path in
    let sim = Event_sim.create tl (circuit tl.library) in
    let sel = List.findi_exn Config.lengths ~f:(fun _ m -> m = n) |> fst in
    Event_sim.set_input_int sim "length_sel" sel ~at:0;
    Event_sim.set_input_int sim "enable" 0 ~at:0;
    Event_sim.set_input_int sim "enable" 1 ~at:(Event_sim.of_ns 5.);
    Event_sim.add_clock sim "clock" ~period:clock_period;
    (* Edges of node 0, and the last change of each counter after them. *)
    let rises = Queue.create () in
    let falls = Queue.create () in
    Event_sim.on_change sim "node0" (fun ~time v ->
      match v with
      | V1 -> Queue.enqueue rises time
      | V0 -> Queue.enqueue falls time
      | X -> ());
    let settle edges = ref 0, edges in
    let settle_a, _ = settle rises in
    let settle_b, _ = settle falls in
    let track name (best, edges) =
      Event_sim.on_change sim name (fun ~time _ ->
        match Queue.peek_back edges with
        | Some e when time >= e -> best := Int.max !best (time - e)
        | _ -> ())
    in
    for i = 0 to R.counter_width - 1 do
      track (Printf.sprintf "count_a[%d]" i) (settle_a, rises);
      track (Printf.sprintf "count_b[%d]" i) (settle_b, falls)
    done;
    (* Sample the decoder's outputs mid-cycle. *)
    let times = Queue.create () in
    let pattern_errors = ref 0 in
    for k = 1 to cycles do
      Event_sim.run_until sim ((k * clock_period) + (clock_period / 2));
      if Option.value (Event_sim.to_int sim "valid") ~default:0 = 1
      then (
        if Option.value (Event_sim.to_int sim "pattern_error") ~default:1 = 1
        then Int.incr pattern_errors;
        Queue.enqueue times (Event_sim.to_int sim "time"))
    done;
    let rise_list = Queue.to_list rises in
    let period =
      (* Average over the second half of the run. *)
      let xs = List.drop rise_list (List.length rise_list / 2) in
      match xs, List.last xs with
      | first :: _, Some last when List.length xs > 1 ->
        Event_sim.to_ps (last - first) /. Float.of_int (List.length xs - 1)
      | _ -> Float.nan
    in
    let stage_ps = period /. Float.of_int (2 * n) in
    let modulus = (1 lsl R.count_width) * 2 * n in
    let deltas =
      Queue.to_list times
      |> List.filter_map ~f:Fn.id
      |> (fun xs -> List.zip_exn (List.drop_last_exn xs) (List.tl_exn xs))
      |> List.map ~f:(fun (a, b) -> (b - a) % modulus)
    in
    let sorted = List.sort deltas ~compare:Int.compare in
    let median = List.nth_exn sorted (List.length sorted / 2) in
    let expected = Float.iround_nearest_exn (20_000. /. stage_ps) in
    let outliers = List.count deltas ~f:(fun d -> abs (d - median) > 2) in
    let unknown = Queue.count times ~f:Option.is_none in
    let thr = R.select_threshold n in
    let settle_stages x = Event_sim.to_ps !x /. stage_ps in
    printf
      "%-4s n=%2d period %7.1f ps (%5.1f ps/stage) | settle A %4.1f B %4.1f stages (threshold %d) | \
       delta median %d (expect ~%d), uncalibrated spread > 2 stages %d/%d, X %d, pattern errors %d, metastable %d\n"
      corner n period stage_ps (settle_stages settle_a) (settle_stages settle_b) thr median expected
      outliers (List.length deltas) unknown !pattern_errors
      (List.length (Event_sim.metastable_events sim))
  ;;


  (* Code-density calibration: samples at clock edges uncorrelated with the
     ring fall in each phase bin in proportion to its width. With bins
     calibrated that way, every 20 ns clock interval should measure 20 ns, to
     within about a bin. *)
  let calibrated ~corner ~path ~n ~cycles =
    let tl = Timing_library.load path in
    let sim = Event_sim.create tl (circuit tl.library) in
    let sel = List.findi_exn Config.lengths ~f:(fun _ m -> m = n) |> fst in
    Event_sim.set_input_int sim "length_sel" sel ~at:0;
    Event_sim.set_input_int sim "enable" 0 ~at:0;
    Event_sim.set_input_int sim "enable" 1 ~at:(Event_sim.of_ns 5.);
    Event_sim.add_clock sim "clock" ~period:clock_period;
    let rises = Queue.create () in
    Event_sim.on_change sim "node0" (fun ~time v ->
      match v with
      | V1 -> Queue.enqueue rises time
      | _ -> ());
    let samples = Queue.create () in
    for k = 1 to cycles do
      Event_sim.run_until sim ((k * clock_period) + (clock_period / 2));
      match
        Event_sim.to_int sim "valid", Event_sim.to_int sim "periods", Event_sim.to_int sim "phase"
      with
      | Some 1, Some k, Some p -> Queue.enqueue samples (k, p)
      | _ -> ()
    done;
    let rises = Queue.to_list rises in
    let period =
      let xs = List.drop rises 10 in
      Event_sim.to_ps (List.last_exn xs - List.hd_exn xs) /. Float.of_int (List.length xs - 1)
    in
    let samples = List.drop (Queue.to_list samples) 5 in
    let bins = 2 * n in
    let hist = Array.create ~len:bins 0 in
    List.iter samples ~f:(fun (_, p) -> hist.(p) <- hist.(p) + 1);
    let total = Float.of_int (List.length samples) in
    let width = Array.map hist ~f:(fun h -> Float.of_int h /. total *. period) in
    let start = Array.create ~len:bins 0. in
    for p = 1 to bins - 1 do
      start.(p) <- start.(p - 1) +. width.(p - 1)
    done;
    let range = 1 lsl R.count_width in
    let time (k, p) = (Float.of_int k *. period) +. start.(p) +. (width.(p) /. 2.) in
    let residuals =
      List.zip_exn (List.drop_last_exn samples) (List.tl_exn samples)
      |> List.map ~f:(fun (a, b) ->
        let dk = (fst b - fst a) % range in
        let d = time (dk, snd b) -. time (0, snd a) in
        d -. 20_000.)
    in
    let rms =
      Float.sqrt (List.sum (module Float) residuals ~f:(fun r -> r *. r) /. Float.of_int (List.length residuals))
    in
    let worst = List.fold residuals ~init:0. ~f:(fun acc r -> Float.max acc (Float.abs r)) in
    let widest = Array.fold width ~init:0. ~f:Float.max in
    let narrowest = Array.fold width ~init:Float.infinity ~f:Float.min in
    printf
      "%-4s n=%d: %d intervals | bins %.0f..%.0f ps | calibrated interval error rms %.1f ps, worst %.1f ps\n"
      corner n (List.length residuals) narrowest widest rms worst
  ;;

end

(* Counter B is clocked through one inverter, counter A through two, so both
   settle ~7-8 stages after their edge. *)
module Selectable = Measure (struct
    let lengths = [ 15; 17; 21 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 9
  end)

module Fixed = Measure (struct
    let lengths = [ 17 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 9
  end)

let%expect_test "ring + clk sampler with Liberty delays at three corners" =
  print_endline "selectable length (mux in the loop):";
  List.iter corners ~f:(fun (corner, path) ->
    List.iter [ 15; 17; 21 ] ~f:(fun n -> Selectable.measure ~corner ~path ~n));
  print_endline "fixed length:";
  List.iter corners ~f:(fun (corner, path) -> Fixed.measure ~corner ~path ~n:17);
  [%expect {|
    selectable length (mux in the loop):
    typ  n=15 period  1567.9 ps ( 52.3 ps/stage) | settle A  6.8 B  6.6 stages (threshold 12) | delta median 383 (expect ~383), uncalibrated spread > 2 stages 197/397, X 0, pattern errors 0, metastable 737
    typ  n=17 period  1726.3 ps ( 50.8 ps/stage) | settle A  7.0 B  6.8 stages (threshold 13) | delta median 394 (expect ~394), uncalibrated spread > 2 stages 83/397, X 0, pattern errors 0, metastable 673
    typ  n=21 period  1793.4 ps ( 42.7 ps/stage) | settle A  8.3 B  8.1 stages (threshold 15) | delta median 469 (expect ~468), uncalibrated spread > 2 stages 76/397, X 0, pattern errors 0, metastable 643
    slow n=15 period  2499.7 ps ( 83.3 ps/stage) | settle A  6.7 B  6.4 stages (threshold 12) | delta median 240 (expect ~240), uncalibrated spread > 2 stages 3/397, X 0, pattern errors 0, metastable 817
    slow n=17 period  2755.7 ps ( 81.0 ps/stage) | settle A  6.8 B  6.6 stages (threshold 13) | delta median 247 (expect ~247), uncalibrated spread > 2 stages 208/397, X 0, pattern errors 0, metastable 720
    slow n=21 period  2862.3 ps ( 68.1 ps/stage) | settle A  8.1 B  7.9 stages (threshold 15) | delta median 294 (expect ~293), uncalibrated spread > 2 stages 19/397, X 0, pattern errors 0, metastable 681
    fast n=15 period  1044.0 ps ( 34.8 ps/stage) | settle A  6.6 B  6.6 stages (threshold 12) | delta median 575 (expect ~575), uncalibrated spread > 2 stages 155/397, X 0, pattern errors 0, metastable 676
    fast n=17 period  1152.2 ps ( 33.9 ps/stage) | settle A  6.8 B  6.8 stages (threshold 13) | delta median 590 (expect ~590), uncalibrated spread > 2 stages 128/397, X 0, pattern errors 0, metastable 593
    fast n=21 period  1203.9 ps ( 28.7 ps/stage) | settle A  8.1 B  8.0 stages (threshold 15) | delta median 698 (expect ~698), uncalibrated spread > 2 stages 58/397, X 0, pattern errors 0, metastable 583
    fixed length:
    typ  n=17 period  1266.9 ps ( 37.3 ps/stage) | settle A  9.5 B  9.3 stages (threshold 13) | delta median 537 (expect ~537), uncalibrated spread > 2 stages 19/397, X 0, pattern errors 0, metastable 757
    slow n=17 period  2017.8 ps ( 59.3 ps/stage) | settle A  9.3 B  9.0 stages (threshold 13) | delta median 337 (expect ~337), uncalibrated spread > 2 stages 21/397, X 0, pattern errors 0, metastable 793
    fast n=17 period   854.7 ps ( 25.1 ps/stage) | settle A  9.2 B  9.1 stages (threshold 13) | delta median 796 (expect ~796), uncalibrated spread > 2 stages 10/397, X 0, pattern errors 0, metastable 678
    |}]
;;

let%expect_test "after code-density calibration, 20 ns clock intervals measure 20 ns" =
  print_endline "selectable length (mux in the loop):";
  List.iter corners ~f:(fun (corner, path) -> Selectable.calibrated ~corner ~path ~n:17 ~cycles:2000);
  print_endline "fixed length:";
  List.iter corners ~f:(fun (corner, path) -> Fixed.calibrated ~corner ~path ~n:17 ~cycles:2000);
  [%expect {|
    selectable length (mux in the loop):
    typ  n=17: 1992 intervals | bins 25..290 ps | calibrated interval error rms 81.7 ps, worst 235.0 ps
    slow n=17: 1992 intervals | bins 41..444 ps | calibrated interval error rms 123.8 ps, worst 387.9 ps
    fast n=17: 1992 intervals | bins 18..193 ps | calibrated interval error rms 50.7 ps, worst 164.4 ps
    fixed length:
    typ  n=17: 1992 intervals | bins 27..78 ps | calibrated interval error rms 38.4 ps, worst 138.8 ps
    slow n=17: 1992 intervals | bins 46..114 ps | calibrated interval error rms 72.1 ps, worst 178.4 ps
    fast n=17: 1992 intervals | bins 16..51 ps | calibrated interval error rms 26.6 ps, worst 86.6 ps
    |}]
;;
