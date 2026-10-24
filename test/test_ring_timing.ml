(* Ring under Event_sim with Liberty delays: the real Ring module (flop-cell
   counters and sampler) at three corners, with and without the length-select
   mux in the loop.

   Counter selection margins are measured from the trace, not assumed. Every
   transition of nodes 0..n-1 and of every counter bit is recorded, with the
   sampler's setup/hold aperture for it (the dfrbpq D constraint tables at
   that transition's slew and the clock slew, as Event_sim applies them):
   - A clock edge at [t] can decode phase [q] only if [t] is in
     [R q = (T q - hold, T (q+1) + setup)], where [T q] is the q-th node
     transition of the wavefront: node [q] may read new and node [q+1] old.
     The majority filter cannot produce anything outside that. Because the
     aperture is wider than a stage, this covers the +-1 stage of phase
     decode uncertainty, and more.
   - A counter is unstable for a clock edge in [(first change - hold, last
     change + setup)] after its own node-0 edge.
   - The margin before a counter's window is the gap between its unstable
     interval and the [R] of the first phase it is read at; the margin after
     is the gap between the [R] of the last phase it is read at and its next
     unstable interval. Both are converted to stages by the mean stage delay.

   Every sample is also checked end to end: its decoded phase is one the
   model allows, its decoded time is that phase's wavefront index, and no
   first-stage flop of the counter it selects went metastable.

   Not modelled: clock skew between sampler flops (ideal clock), wire loads,
   and the counters' next-state logic (Hardcaml primitives, zero delay); the
   asserted margin is what is left for those.

   Findings:
   - counters are safe to read in phases [12, 2n + 6) after their edge (B a
     stage earlier than A) at every corner and length, so the configuration
     is L = 12, E = 6. The old window (E = 0, L = 9) left 0.2-0.9 stages
     after the change;
   - centred windows leave >= 5 stages on each side for the fixed ring; with
     the mux, the mux bin lands on one side, so the other keeps 4.5;
   - the bins ending at node 0 (NAND, and its load) are 2-2.5x an inverter
     stage; with the mux, 5-8x;
   - after code-density calibration, held-out intervals measure right to
     28-68 ps rms (fixed length) or 44-121 ps (mux): prefer a fixed length;
     escape injection locking by changing the clock frequency instead. *)

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

(* Asserted at every corner and length, on both sides of both windows. *)
let min_margin_stages = 4.

(* The sampler's aperture (setup, hold) for a transition on its D pin, signed,
   in fs: the data must be stable from edge - setup to edge + hold. Worst over
   the arcs, as Event_sim takes it. *)
let aperture (tl : Timing_library.t) ~rising ~d_slew ~clock_slew =
  let cell = Map.find_exn tl.cells dfrbpq_cell in
  let worst prefix =
    Timing_library.Cell.constraints cell ~input:"D"
    |> List.filter_map ~f:(fun (a : Timing_library.Arc.t) ->
      if String.is_prefix a.timing_type ~prefix
      then
        Option.map
          (if rising then a.rise_constraint else a.fall_constraint)
          ~f:(fun t -> Timing_library.Table.lookup t d_slew clock_slew)
      else None)
    |> List.max_elt ~compare:Float.compare
    |> Option.value ~default:0.
  in
  Event_sim.of_ns (worst "setup_"), Event_sim.of_ns (worst "hold_")
;;

(* A recorded transition, with the sampler aperture around it. *)
type transition =
  { time : int
  ; setup : int
  ; hold : int
  }

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

  let create_sim ~path ~n ~clock_period =
    let tl = Timing_library.load path in
    let sim = Event_sim.create tl (circuit tl.library) in
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
  let margins ~corner ~path ~n =
    let tl, sim = create_sim ~path ~n ~clock_period in
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
            aperture tl ~rising ~d_slew:(Event_sim.slew_ns sim name) ~clock_slew:(clock_slew ())
          in
          f { time; setup; hold } rising))
    in
    (* Node transitions of the wavefront, in order. *)
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
    (* Index the wavefront from the first rise of node 0: phase q mod 2n. *)
    let nodes =
      Queue.to_list nodes
      |> List.sort ~compare:(fun (_, _, a) (_, _, b) -> Int.compare a.time b.time)
      |> List.drop_while ~f:(fun (i, rising, _) -> not (i = 0 && rising))
      |> Array.of_list
    in
    Array.iteri nodes ~f:(fun q (i, rising, _) ->
      (* After the rise (p < n), u_i = node_i xor odd(i) becomes 1. *)
      let expect_rising = Bool.equal (q % (2 * n) < n) (i % 2 = 0) in
      if i <> q % n || Bool.( <> ) rising expect_rising
      then raise_s [%message "not a single wavefront" (q : int) (i : int)]);
    let tr = Array.map nodes ~f:(fun (_, _, t) -> t) in
    let len = Array.length tr in
    let periods = (len - 1) / (2 * n) in
    (* Bin widths as the sampler sees them, averaged per phase. *)
    let bin p =
      let qs = List.filter (List.range 0 (periods * 2 * n)) ~f:(fun q -> q % (2 * n) = p) in
      let seen q = tr.(q).time + tr.(q).setup in
      List.sum (module Int) qs ~f:(fun q -> seen (q + 1) - seen q)
      |> fun total -> Event_sim.to_ps total /. Float.of_int (List.length qs)
    in
    let bins = List.init (2 * n) ~f:bin in
    (* Margins are in stage delays: the mean bin of the inverter stages. The
       NAND bins (ending at node 0, with the mux if any) are several stages
       wide. *)
    let stage =
      List.filteri bins ~f:(fun p _ -> p % n <> n - 1)
      |> List.sum (module Float) ~f:Fn.id
      |> fun total -> Float.of_int (Event_sim.of_ps total) /. Float.of_int ((2 * n) - 2)
    in
    let stages x = Float.of_int x /. stage in
    (* When a clock edge can decode phase q. Node i can read new after
       [T i - hold] and old before [T i + setup]. A filtered position takes a
       value only if two of its three raw neighbours can, so phase q needs
       that for "new" at positions up to q and "old" from q+1. *)
    let can_new i = tr.(i).time - tr.(i).hold in
    let can_old i = tr.(i).time + tr.(i).setup in
    let median3 f i =
      match List.sort [ f (i - 1); f i; f (i + 1) ] ~compare:Int.compare with
      | [ _; m; _ ] -> m
      | _ -> assert false
    in
    let r_lo q = Int.max (median3 can_new q) (median3 can_new (q - 1)) in
    let r_hi q = Int.min (median3 can_old (q + 1)) (median3 can_old (q + 2)) in
    let s = R.select_threshold n in
    (* For a counter with edges at q = first + 2n j: per edge, the unstable
       interval; then the window [q + s, q + s + n) against it and the next. *)
    let analyse ~first changes =
      let edges = Array.init periods ~f:(fun j -> first + (2 * n * j)) in
      let unstable = Array.create ~len:periods None in
      Queue.iter changes ~f:(fun c ->
        (* the edge this change follows *)
        match Array.binary_search edges `Last_less_than_or_equal_to c.time ~compare:(fun q t ->
                Int.compare tr.(q).time t) with
        | None -> ()
        | Some j ->
          let lo = c.time - c.hold
          and hi = c.time + c.setup in
          unstable.(j)
          <- Some
               (match unstable.(j) with
                | None -> lo, hi
                | Some (lo', hi') -> Int.min lo lo', Int.max hi hi'));
      (* Phases (relative to the edge, so negative is the period before)
         that can overlap the unstable interval: the counter is safe to read
         in [last + 1, 2n + first). *)
      let first_unsafe = ref Int.max_value
      and last_unsafe = ref Int.min_value
      and before = ref Int.max_value
      and after = ref Int.max_value in
      for j = 1 to periods - 2 do
        match unstable.(j), unstable.(j + 1) with
        | Some (lo, hi), Some (lo_next, _) when edges.(j) + s + n + 4 < len ->
          for d = -n to n - 1 do
            let q = edges.(j) + d in
            if r_lo q < hi && r_hi q > lo
            then (
              first_unsafe := Int.min !first_unsafe d;
              last_unsafe := Int.max !last_unsafe d)
          done;
          for q = edges.(j) + s to edges.(j) + s + n - 1 do
            before := Int.min !before (r_lo q - hi);
            after := Int.min !after (lo_next - r_hi q)
          done
        | _ -> ()
      done;
      !last_unsafe + 1, !first_unsafe, stages !before, stages !after
    in
    let a = analyse ~first:0 changes_a in
    let b = analyse ~first:n changes_b in
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
            Array.binary_search tr `Last_less_than_or_equal_to e ~compare:(fun x e ->
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
        |> List.find ~f:(fun q' -> q' % (2 * n) = p && r_lo q' < e && e < r_hi q')
        |> Option.map ~f:(fun q' -> q', t))
    in
    let impossible_phase = List.count matched ~f:Option.is_none in
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
    let others =
      List.filteri bins ~f:(fun p _ -> not (List.mem [ 0; n - 1; n; (2 * n) - 1 ] p ~equal:Int.equal))
    in
    let lo_others = List.fold others ~init:Float.infinity ~f:Float.min in
    let hi_others = List.fold others ~init:0. ~f:Float.max in
    let sa_max, sa_min, a_before, a_after = a in
    let sb_max, sb_min, b_before, b_after = b in
    let worst = List.fold [ a_before; a_after; b_before; b_after ] ~init:Float.infinity ~f:Float.min in
    printf
      "%-4s n=%2d period %4.0f ps, stage %4.1f ps | safe phases A [%2d, %d%+d) B [%2d, %d%+d) | s=%2d margins A %4.1f/%4.1f B %4.1f/%4.1f stages\n"
      corner n
      (Event_sim.to_ps (tr.(periods * 2 * n).time - tr.(0).time) /. Float.of_int periods)
      (Event_sim.to_ps (Float.iround_nearest_exn stage)) sa_max (2 * n) sa_min sb_max (2 * n)
      sb_min s a_before a_after b_before b_after;
    printf
      "     bins: NAND %3.0f/%3.0f ps, after node 0 %2.0f/%2.0f, others %2.0f..%2.0f | %d samples: X %d, pattern errors %d, outside model %d, time wrong %d, selected counter caught %d, counter metastable %d\n"
      (List.nth_exn bins (n - 1)) (List.nth_exn bins ((2 * n) - 1)) (List.nth_exn bins 0)
      (List.nth_exn bins n) lo_others hi_others (List.length samples) unknown pattern_errors
      impossible_phase time_errors caught counter_meta;
    worst
  ;;

  (* Code-density calibration: samples at clock edges uncorrelated with the
     ring fall in each phase bin in proportion to its width. Bins and the ring
     period are calibrated from one run (20 ns clock; the period from the
     mean decoded interval), then applied to an independent run with a
     different clock period, whose every interval should measure that
     period to within about a bin. *)
  let calibrated ~corner ~path ~n =
    let run ~clock_period ~cycles =
      let _, sim = create_sim ~path ~n ~clock_period in
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

let%expect_test "counter selection margins with Liberty delays at three corners" =
  print_endline "selectable length (mux in the loop):";
  List.concat_map corners ~f:(fun (corner, path) ->
    List.map [ 15; 17; 21 ] ~f:(fun n -> Selectable.margins ~corner ~path ~n))
  |> List.fold ~init:Float.infinity ~f:Float.min
  |> check;
  print_endline "fixed length:";
  List.map corners ~f:(fun (corner, path) -> Fixed.margins ~corner ~path ~n:17)
  |> List.fold ~init:Float.infinity ~f:Float.min
  |> check;
  [%expect {|
    selectable length (mux in the loop):
    typ  n=15 period 1568 ps, stage 35.2 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 12.0/ 5.2 B 13.7/ 4.8 stages
         bins: NAND 281/302 ps, after node 0 31/40, others 29..52 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    typ  n=17 period 1726 ps, stage 35.8 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 13.9/ 6.2 B 13.9/ 5.5 stages
         bins: NAND 279/302 ps, after node 0 31/40, others 29..52 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    typ  n=21 period 1793 ps, stage 35.5 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  8.9/12.4 B  8.5/11.9 stages
         bins: NAND 185/188 ps, after node 0 31/40, others 29..52 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=15 period 2500 ps, stage 55.8 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 12.1/ 4.8 B 13.8/ 4.5 stages
         bins: NAND 446/490 ps, after node 0 39/72, others 36..92 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=17 period 2756 ps, stage 56.9 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 14.1/ 6.0 B 13.9/ 5.0 stages
         bins: NAND 446/490 ps, after node 0 39/72, others 36..93 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=21 period 2862 ps, stage 56.4 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  8.9/12.2 B  8.5/11.6 stages
         bins: NAND 296/309 ps, after node 0 39/72, others 36..93 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=15 period 1044 ps, stage 23.7 ps | safe phases A [12, 30+7) B [11, 30+6) | s=17 margins A 12.0/ 5.0 B 13.5/ 5.1 stages
         bins: NAND 177/202 ps, after node 0 13/34, others 12..41 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=17 period 1152 ps, stage 24.1 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A 14.1/ 6.3 B 13.6/ 5.5 stages
         bins: NAND 178/202 ps, after node 0 13/34, others 12..42 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=21 period 1204 ps, stage 24.0 ps | safe phases A [12, 42+7) B [11, 42+6) | s=20 margins A  9.2/12.3 B  8.6/11.9 stages
         bins: NAND 117/128 ps, after node 0 13/34, others 12..42 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    every margin >= 4 stages: true
    fixed length:
    typ  n=17 period 1267 ps, stage 34.4 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  8.4/ 6.5 B  7.1/ 5.8 stages
         bins: NAND  94/ 72 ps, after node 0 30/40, others 29..40 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    slow n=17 period 2018 ps, stage 54.6 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  8.5/ 6.2 B  7.1/ 5.2 stages
         bins: NAND 146/124 ps, after node 0 39/72, others 36..74 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    fast n=17 period  855 ps, stage 23.3 ps | safe phases A [12, 34+7) B [11, 34+6) | s=18 margins A  8.7/ 6.6 B  7.2/ 5.7 stages
         bins: NAND  56/ 54 ps, after node 0 13/34, others 12..34 | 393 samples: X 0, pattern errors 0, outside model 0, time wrong 0, selected counter caught 0, counter metastable 0
    every margin >= 4 stages: true
    |}]
;;

let%expect_test "after code-density calibration, held-out clock intervals measure right" =
  print_endline "selectable length (mux in the loop):";
  List.iter corners ~f:(fun (corner, path) -> Selectable.calibrated ~corner ~path ~n:17);
  print_endline "fixed length:";
  List.iter corners ~f:(fun (corner, path) -> Fixed.calibrated ~corner ~path ~n:17);
  [%expect {|
    selectable length (mux in the loop):
    typ  n=17: period 1726.3 ps | bins 25..290 ps | 992 held-out intervals: error rms 63.5 ps, worst 210.3 ps
    slow n=17: period 2755.7 ps | bins 41..444 ps | 992 held-out intervals: error rms 121.4 ps, worst 342.8 ps
    fast n=17: period 1152.2 ps | bins 18..193 ps | 992 held-out intervals: error rms 43.5 ps, worst 159.7 ps
    fixed length:
    typ  n=17: period 1266.8 ps | bins 27..78 ps | 992 held-out intervals: error rms 42.4 ps, worst 119.8 ps
    slow n=17: period 2017.8 ps | bins 46..114 ps | 992 held-out intervals: error rms 67.5 ps, worst 190.4 ps
    fast n=17: period 854.7 ps | bins 16..51 ps | 992 held-out intervals: error rms 28.1 ps, worst 78.2 ps
    |}]
;;
