(* Ring under Event_sim with Liberty delays: the real Ring module (flop-cell
   counters and sampler) at three corners, with and without the length-select
   mux in the loop.

   Counter selection margins are measured from the trace, not assumed
   (test/ring_margins.ml, shared with Pin_capture's capture slots). Every
   transition of nodes 0..n-1 and of every counter bit is recorded, with the
   sampler's setup/hold aperture for it (the dfrbpq D constraint tables at
   that transition's slew and the clock slew, as Event_sim applies them):
   - Let [T q] be the q-th node transition of the wavefront. A clock edge at
     [t] can read node [q] new if [t > T q - hold], and old if [t < T q +
     setup]. A majority-filtered position takes a value only if two of its
     three raw inputs can, so phase [q] (positions up to [q] new, from [q+1]
     old) is possible only in an interval [R q] built from medians of three.
     Because the aperture is wider than a stage, this covers the +-1 stage of
     phase decode uncertainty, and more.
   - A counter is unstable for a clock edge in [(first change - hold, last
     change + setup)] after its own node-0 edge, over all its bits.
   - The margin before a counter's window is the gap between its unstable
     interval and the [R] of the first phase it is read at; the margin after
     is the gap between the [R] of the last phase it is read at and its next
     unstable interval. Both are converted to stages by the mean inverter
     stage delay.

   Every sample is also checked end to end: its decoded phase is one the
   model allows, its decoded time is that phase's wavefront index, and no
   first-stage flop of the counter it selects went metastable.

   Wires: the reported numbers use the library's default wire load ("1k",
   pre-layout); flat 1 fF wires are kept as a regression. Not modelled: clock
   skew between sampler flops (ideal clock), wire resistance, and the
   counters' next-state logic (Hardcaml primitives, zero delay); the asserted
   margin is what is left for those.

   Findings (Liberty 1k wire load unless stated):
   - counters are safe to read in phases [12, 2n + 6) after their edge (B a
     stage earlier than A) at every corner and length, with either wire
     model, so the configuration is L = 12, E = 6. The old window (E = 0, L =
     9) left 0.2-0.9 stages after the change. A ring loaded by more than its
     sampler has slower stages and smaller L and E (Pin_capture's ring: L =
     9, E = 4, test/test_pin_capture_timing.ml);
   - centred windows leave >= 5.6 stages on each side for the fixed ring
     (>= 5.2 flat); with the mux, the mux bin lands on one side, so the other
     keeps 4.7;
   - the bins ending at node 0 (NAND, and its load) are 2-3x an inverter
     stage; with the mux, 5-7x;
   - after code-density calibration, held-out intervals measure right to
     29-84 ps rms (fixed length; 28-68 ps flat) or 55-128 ps (mux): prefer a
     fixed length; escape injection locking by changing the clock frequency
     instead. *)

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

(* For the end-to-end checks: not close to a simple multiple of any ring
   period here, so the samples visit every phase (checked). *)
let sample_period = Event_sim.of_ps 19_937.

(* Asserted at every corner and length, on both sides of both windows. *)
let min_margin_stages = 4.

module Measure (Config : Ring.Timed_config) = struct
  module R = Ring.Make_timed (Config)

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
      (Signal.output "count_a" state.count_a
       :: Signal.output "count_b" state.count_b
       :: R.Clk_sampler.O.(to_list (map2 port_names o ~f:Signal.output)))
  ;;

  let create_sim ~wire_load ~path ~n ~clock_period =
    let tl = Timing_library.load path in
    let sim = Event_sim.create ~config:{ Event_sim.Config.default with wire_load } tl (circuit tl.library) in
    let sel = List.findi_exn Config.lengths ~f:(fun _ m -> m = n) |> fst in
    Event_sim.set_input_int sim "length_sel" sel ~at:0;
    Event_sim.set_input_int sim "enable" 0 ~at:0;
    Event_sim.set_input_int sim "enable" 1 ~at:(Event_sim.of_ns 5.);
    Event_sim.add_clock sim "clock" ~period:clock_period;
    tl, sim
  ;;

  (* Ring phases [p] in counter A's window (B is read in the others). *)
  let reads_a ~n p = (p - R.select_threshold n) % (2 * n) < n

  let cycles = 400
  let warmup = Event_sim.of_ns 100.

  (* Margins for one length at one corner; returns the smallest, in stages. *)
  let margins ~wire_load ~corner ~path ~n =
    let clock_period = sample_period in
    let tl, sim = create_sim ~wire_load ~path ~n ~clock_period in
    let clock_slew () = Event_sim.slew_ns sim "clock" in
    let record name ~f =
      Event_sim.on_change sim name (fun ~time v ->
        if time >= warmup
        then (
          let rising =
            match v with
            | V1 -> true
            | V0 -> false
            | X -> raise_s [%message "X after warm-up" (name : string) (time : int)]
          in
          let setup, hold =
            Ring_margins.aperture tl ~cell:dfrbpq_cell ~rising
              ~d_slew:(Event_sim.slew_ns sim name) ~clock_slew:(clock_slew ())
          in
          f { Ring_margins.time; setup; hold } rising))
    in
    (* Node transitions of the wavefront. *)
    let nodes = Queue.create () in
    for i = 0 to n - 1 do
      record (Printf.sprintf "ring_node%d" i) ~f:(fun t rising -> Queue.enqueue nodes (i, rising, t))
    done;
    let changes_a = Queue.create () in
    let changes_b = Queue.create () in
    for i = 0 to R.counter_width - 1 do
      record (Printf.sprintf "count_a[%d]" i) ~f:(fun t _ -> Queue.enqueue changes_a t);
      record (Printf.sprintf "count_b[%d]" i) ~f:(fun t _ -> Queue.enqueue changes_b t)
    done;
    (* The decoder's output for the first-stage edge two cycles earlier. *)
    let samples = Queue.create () in
    for k = 1 to cycles do
      Event_sim.run_until sim ((k * clock_period) + (clock_period / 2));
      let get name = Event_sim.to_int sim name in
      if Option.value (get "valid") ~default:0 = 1
      then Queue.enqueue samples ((k - 2) * clock_period, get "phase", get "time", get "pattern_error")
    done;
    let w = Ring_margins.of_node_transitions ~n (Queue.to_list nodes) in
    let tr = w.tr in
    let len = Array.length tr in
    let s = R.select_threshold n in
    let a = Ring_margins.counter w ~first:0 ~s (Queue.to_list changes_a) in
    let b = Ring_margins.counter w ~first:n ~s (Queue.to_list changes_b) in
    (* End to end, every sample: the decoded phase is one the model allows,
       the decoded time is that phase's wavefront index (up to a constant
       offset), and the counter it selects was not caught changing. *)
    let samples =
      Queue.to_list samples
      |> List.filter ~f:(fun (e, _, _, _) -> e > tr.(12).time && e < tr.(len - 12).time)
    in
    let unknown = List.count samples ~f:(fun (_, p, t, _) -> Option.is_none p || Option.is_none t) in
    let pattern_errors = List.count samples ~f:(fun (_, _, _, err) -> Option.value err ~default:1 = 1) in
    let samples =
      List.filter_map samples ~f:(fun (e, p, t, _) ->
        match p, t with
        | Some p, Some t ->
          let q =
            Array.binary_search tr `Last_less_than_or_equal_to e ~compare:(fun (x : Ring_margins.transition) e ->
              Int.compare x.time e)
            |> Option.value_exn
          in
          Some (e, q, p, t)
        | _ -> None)
    in
    (* The model's phase index for each sample, if it allows the decoded
       phase at all. *)
    let matched =
      List.map samples ~f:(fun (e, q, p, t) ->
        List.range (q - 8) (q + 9)
        |> List.find ~f:(fun q' ->
          q' % (2 * n) = p && Ring_margins.r_lo w q' < e && e < Ring_margins.r_hi w q')
        |> Option.map ~f:(fun q' -> q', t))
    in
    let impossible_phase = List.count matched ~f:Option.is_none in
    let visited =
      List.map samples ~f:(fun (_, _, p, _) -> p) |> List.dedup_and_sort ~compare:Int.compare |> List.length
    in
    let modulus = (1 lsl R.count_width) * 2 * n in
    let offsets = List.filter_map matched ~f:(Option.map ~f:(fun (q, t) -> (t - q) % modulus)) in
    let mode =
      List.sort_and_group offsets ~compare:Int.compare
      |> List.max_elt ~compare:(fun a b -> Int.compare (List.length a) (List.length b))
      |> Option.value_map ~default:0 ~f:List.hd_exn
    in
    let time_errors = List.count offsets ~f:(fun d -> d <> mode) in
    let caught =
      let meta = Event_sim.metastable_events sim in
      List.count samples ~f:(fun (e, _, p, _) ->
        let prefix = if reads_a ~n p then "ring_sample_count_a_" else "ring_sample_count_b_" in
        List.exists meta ~f:(fun m -> m.edge_time = e && String.is_prefix m.instance ~prefix))
    in
    let counter_meta =
      List.count (Event_sim.metastable_events sim) ~f:(fun m ->
        String.is_prefix m.instance ~prefix:"ring_cnt_")
    in
    printf
      "%-4s n=%2d period %4.0f ps, stage %4.1f ps | %s\n"
      corner n
      (Event_sim.to_ps (tr.(w.periods * 2 * n).time - tr.(0).time) /. Float.of_int w.periods)
      (Event_sim.to_ps (Float.iround_nearest_exn w.stage))
      (Ring_margins.pp_counters w ~s a b);
    printf
      "     %s | %d samples, %d/%d phases: X %d, pattern errors %d, outside model %d, time wrong %d, selected counter caught %d, counter metastable %d\n"
      (Ring_margins.pp_bins w) (List.length samples) visited (2 * n) unknown pattern_errors
      impossible_phase time_errors caught counter_meta;
    Ring_margins.worst a b
  ;;

  (* Code-density calibration: samples at clock edges uncorrelated with the
     ring fall in each phase bin in proportion to its width. Bins and the ring
     period are calibrated from one run (20 ns clock; the period from the
     mean decoded interval), then applied to an independent run with a
     different clock period, whose every interval should measure that
     period to within about a bin. *)
  let calibrated ~wire_load ~corner ~path ~n =
    let run ~clock_period ~cycles =
      let _, sim = create_sim ~wire_load ~path ~n ~clock_period in
      let samples = Queue.create () in
      for k = 1 to cycles do
        Event_sim.run_until sim ((k * clock_period) + (clock_period / 2));
        match
          Event_sim.to_int sim "valid", Event_sim.to_int sim "periods", Event_sim.to_int sim "phase"
        with
        | Some 1, Some k, Some p -> Queue.enqueue samples (k, p)
        | _ -> ()
      done;
      List.drop (Queue.to_list samples) 5
    in
    let bins = 2 * n in
    let range = 1 lsl R.count_width in
    let intervals samples = List.zip_exn (List.drop_last_exn samples) (List.tl_exn samples) in
    (* Calibration run. *)
    let cal = run ~clock_period ~cycles:2000 in
    let hist = Array.create ~len:bins 0 in
    List.iter cal ~f:(fun (_, p) -> hist.(p) <- hist.(p) + 1);
    let total = Float.of_int (List.length cal) in
    (* Bin p as a fraction of the period: [start p, start p + width p). *)
    let width = Array.map hist ~f:(fun h -> Float.of_int h /. total) in
    let start = Array.create ~len:bins 0. in
    for p = 1 to bins - 1 do
      start.(p) <- start.(p - 1) +. width.(p - 1)
    done;
    let ring_periods ((ka, pa), (kb, pb)) =
      Float.of_int ((kb - ka) % range) +. start.(pb) +. (width.(pb) /. 2.) -. start.(pa) -. (width.(pa) /. 2.)
    in
    let period =
      let iv = intervals cal in
      Event_sim.to_ps clock_period *. Float.of_int (List.length iv)
      /. List.sum (module Float) iv ~f:ring_periods
    in
    (* Held-out run: another clock period, so other phases. *)
    let val_period = Event_sim.of_ps 17_310. in
    let residuals =
      intervals (run ~clock_period:val_period ~cycles:1000)
      |> List.map ~f:(fun iv -> (ring_periods iv *. period) -. Event_sim.to_ps val_period)
    in
    let rms =
      Float.sqrt
        (List.sum (module Float) residuals ~f:(fun r -> r *. r) /. Float.of_int (List.length residuals))
    in
    let worst = List.fold residuals ~init:0. ~f:(fun acc r -> Float.max acc (Float.abs r)) in
    let widest = Array.fold width ~init:0. ~f:Float.max *. period in
    let narrowest = Array.fold width ~init:Float.infinity ~f:Float.min *. period in
    printf
      "%-4s n=%d: period %.1f ps | bins %.0f..%.0f ps | %d held-out intervals: error rms %.1f ps, worst %.1f ps\n"
      corner n period narrowest widest (List.length residuals) rms worst
  ;;
end

(* L and E from the safe phases measured below. *)
module Selectable = Measure (struct
    let lengths = [ 15; 17; 21 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 12
    let counter_earliest_change_stages = 6
  end)

module Fixed = Measure (struct
    let lengths = [ 17 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 12
    let counter_earliest_change_stages = 6
  end)

let check worst =
  printf "every margin >= %.0f stages: %b\n" min_margin_stages (Float.( >= ) worst min_margin_stages)
;;

(* The baseline for reported numbers: the library's default wire load
   ("1k"), pre-layout. [Flat] (1 fF per net, Event_sim's default) is kept as
   a regression. *)
let wire_load : Event_sim.Config.wire_load = Liberty_default

let%expect_test "counter selection margins with Liberty delays at three corners (Liberty 1k wire load)" =
  print_endline "selectable length (mux in the loop):";
  List.concat_map corners ~f:(fun (corner, path) ->
    List.map [ 15; 17; 21 ] ~f:(fun n -> Selectable.margins ~wire_load ~corner ~path ~n))
  |> List.fold ~init:Float.infinity ~f:Float.min
  |> check;
  print_endline "fixed length:";
  List.map corners ~f:(fun (corner, path) -> Fixed.margins ~wire_load ~corner ~path ~n:17)
  |> List.fold ~init:Float.infinity ~f:Float.min
  |> check;
  [%expect {|
    selectable length (mux in the loop):
    typ  n=15 period 2119 ps, stage 50.8 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 11.5/ 5.1 B 12.2/ 4.9 stages
         bins: NAND 346/349 ps, after node 0 48/56, others 43..78 | 393 samples, 30/30 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    typ  n=17 period 2360 ps, stage 52.1 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 13.1/ 6.1 B 12.6/ 5.7 stages
         bins: NAND 344/349 ps, after node 0 48/56, others 43..79 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    typ  n=21 period 2537 ps, stage 51.6 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  9.4/11.5 B  8.9/11.6 stages
         bins: NAND 244/230 ps, after node 0 48/56, others 43..79 | 393 samples, 42/42 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=15 period 3420 ps, stage 82.1 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 11.5/ 4.8 B 12.2/ 4.7 stages
         bins: NAND 557/564 ps, after node 0 63/94, others 60..136 | 393 samples, 30/30 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=17 period 3814 ps, stage 84.1 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 13.3/ 5.9 B 12.6/ 5.3 stages
         bins: NAND 558/564 ps, after node 0 63/94, others 60..137 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=21 period 4108 ps, stage 83.3 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  9.5/11.2 B  9.0/11.3 stages
         bins: NAND 399/377 ps, after node 0 63/94, others 60..137 | 393 samples, 42/42 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=15 period 1377 ps, stage 33.1 ps | safe phases A [12, 30+7) B [11, 30+7) | s=17 margins A 11.5/ 5.1 B 12.0/ 5.3 stages
         bins: NAND 218/231 ps, after node 0 24/45, others 21..57 | 393 samples, 30/30 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=17 period 1535 ps, stage 33.9 ps | safe phases A [12, 34+7) B [11, 34+7) | s=18 margins A 13.3/ 6.3 B 12.4/ 5.8 stages
         bins: NAND 219/230 ps, after node 0 24/45, others 21..58 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=21 period 1651 ps, stage 33.6 ps | safe phases A [12, 42+7) B [11, 42+7) | s=20 margins A  9.4/11.6 B  8.8/11.8 stages
         bins: NAND 155/153 ps, after node 0 24/45, others 21..58 | 393 samples, 42/42 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    every margin >= 4 stages: true
    fixed length:
    typ  n=17 period 1836 ps, stage 49.3 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  8.9/ 6.4 B  7.7/ 6.0 stages
         bins: NAND 148/110 ps, after node 0 48/56, others 43..56 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=17 period 2974 ps, stage 79.6 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  9.1/ 6.2 B  7.7/ 5.6 stages
         bins: NAND 241/186 ps, after node 0 63/94, others 60..99 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=17 period 1197 ps, stage 32.2 ps | safe phases A [12, 34+7) B [11, 34+7) | s=18 margins A  9.1/ 6.6 B  7.5/ 6.1 stages
         bins: NAND  91/ 77 ps, after node 0 24/45, others 21..43 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    every margin >= 4 stages: true
    |}]
;;

let%expect_test "after code-density calibration, held-out clock intervals measure right (Liberty 1k wire load)" =
  print_endline "selectable length (mux in the loop):";
  List.iter corners ~f:(fun (corner, path) -> Selectable.calibrated ~wire_load ~corner ~path ~n:17);
  print_endline "fixed length:";
  List.iter corners ~f:(fun (corner, path) -> Fixed.calibrated ~wire_load ~corner ~path ~n:17);
  [%expect {|
    selectable length (mux in the loop):
    typ  n=17: period 2360.0 ps | bins 40..345 ps | 992 held-out intervals: error rms 90.5 ps, worst 384.3 ps
    slow n=17: period 3814.2 ps | bins 63..566 ps | 992 held-out intervals: error rms 127.9 ps, worst 380.0 ps
    fast n=17: period 1534.8 ps | bins 15..229 ps | 992 held-out intervals: error rms 55.2 ps, worst 149.5 ps
    fixed length:
    typ  n=17: period 1835.5 ps | bins 39..131 ps | 992 held-out intervals: error rms 44.0 ps, worst 132.5 ps
    slow n=17: period 2974.4 ps | bins 64..207 ps | 992 held-out intervals: error rms 83.7 ps, worst 229.1 ps
    fast n=17: period 1196.8 ps | bins 23..81 ps | 992 held-out intervals: error rms 29.2 ps, worst 104.8 ps
    |}]
;;

let%expect_test "regression, flat 1 fF wires: margins and calibration" =
  let wire_load : Event_sim.Config.wire_load = Flat in
  print_endline "selectable length (mux in the loop):";
  List.concat_map corners ~f:(fun (corner, path) ->
    List.map [ 15; 17; 21 ] ~f:(fun n -> Selectable.margins ~wire_load ~corner ~path ~n))
  |> List.fold ~init:Float.infinity ~f:Float.min
  |> check;
  List.iter corners ~f:(fun (corner, path) -> Selectable.calibrated ~wire_load ~corner ~path ~n:17);
  print_endline "fixed length:";
  List.map corners ~f:(fun (corner, path) -> Fixed.margins ~wire_load ~corner ~path ~n:17)
  |> List.fold ~init:Float.infinity ~f:Float.min
  |> check;
  List.iter corners ~f:(fun (corner, path) -> Fixed.calibrated ~wire_load ~corner ~path ~n:17);
  [%expect {|
    selectable length (mux in the loop):
    typ  n=15 period 1568 ps, stage 35.2 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 12.0/ 5.2 B 13.7/ 4.8 stages
         bins: NAND 281/302 ps, after node 0 31/40, others 29..52 | 393 samples, 30/30 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    typ  n=17 period 1726 ps, stage 35.8 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 13.9/ 6.2 B 13.9/ 5.5 stages
         bins: NAND 279/302 ps, after node 0 31/40, others 29..52 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    typ  n=21 period 1793 ps, stage 35.5 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  8.9/12.4 B  8.5/11.9 stages
         bins: NAND 185/188 ps, after node 0 31/40, others 29..52 | 393 samples, 42/42 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=15 period 2500 ps, stage 55.8 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 12.1/ 4.8 B 13.8/ 4.5 stages
         bins: NAND 446/490 ps, after node 0 39/72, others 36..92 | 393 samples, 30/30 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=17 period 2756 ps, stage 56.9 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 14.1/ 6.0 B 13.9/ 5.0 stages
         bins: NAND 446/490 ps, after node 0 39/72, others 36..93 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=21 period 2862 ps, stage 56.4 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  8.9/12.2 B  8.5/11.6 stages
         bins: NAND 296/309 ps, after node 0 39/72, others 36..93 | 393 samples, 42/42 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=15 period 1044 ps, stage 23.7 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 12.0/ 5.0 B 13.5/ 5.1 stages
         bins: NAND 177/202 ps, after node 0 13/34, others 12..41 | 393 samples, 30/30 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=17 period 1152 ps, stage 24.1 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 14.1/ 6.3 B 13.6/ 5.5 stages
         bins: NAND 178/202 ps, after node 0 13/34, others 12..42 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=21 period 1204 ps, stage 24.0 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  9.2/12.3 B  8.6/11.9 stages
         bins: NAND 117/128 ps, after node 0 13/34, others 12..42 | 393 samples, 42/42 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    every margin >= 4 stages: true
    typ  n=17: period 1726.3 ps | bins 25..290 ps | 992 held-out intervals: error rms 63.5 ps, worst 210.3 ps
    slow n=17: period 2755.7 ps | bins 41..444 ps | 992 held-out intervals: error rms 121.4 ps, worst 342.8 ps
    fast n=17: period 1152.2 ps | bins 18..193 ps | 992 held-out intervals: error rms 43.5 ps, worst 159.7 ps
    fixed length:
    typ  n=17 period 1267 ps, stage 34.4 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  8.4/ 6.5 B  7.1/ 5.8 stages
         bins: NAND  94/ 72 ps, after node 0 30/40, others 29..40 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=17 period 2018 ps, stage 54.6 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  8.5/ 6.2 B  7.1/ 5.2 stages
         bins: NAND 146/124 ps, after node 0 39/72, others 36..74 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=17 period  855 ps, stage 23.3 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  8.7/ 6.6 B  7.2/ 5.7 stages
         bins: NAND  56/ 54 ps, after node 0 13/34, others 12..34 | 393 samples, 34/34 phases: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    every margin >= 4 stages: true
    typ  n=17: period 1266.8 ps | bins 27..78 ps | 992 held-out intervals: error rms 42.4 ps, worst 119.8 ps
    slow n=17: period 2017.8 ps | bins 46..114 ps | 992 held-out intervals: error rms 67.5 ps, worst 190.4 ps
    fast n=17: period 854.7 ps | bins 16..51 ps | 992 held-out intervals: error rms 28.1 ps, worst 78.2 ps
    |}]
;;
