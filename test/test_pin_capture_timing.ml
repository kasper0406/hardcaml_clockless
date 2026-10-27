(* Pin_capture under Event_sim with Liberty delays at three corners: the real
   front (hand-placed NAND/XOR/clock trees, flop-cell slots and pointers),
   a fixed 17-stage ring with flop-cell counters, flop-cell synchroniser
   first stages, drained at 50 MHz. Expectations come from the true pin
   edges (Capture_oracle), not from the design. The long gate run is
   test/gate/capture_gate.exe. Reported numbers use the library's default
   wire load ("1k", pre-layout); flat 1 fF wires are kept as a regression.

   Findings recorded by these tests (and the 105k-edge gate run), Liberty 1k
   wire load:
   - counter selection: the ring here also drives the capture's data
     buffers, so its stages are slower than a bare ring's (72 ps typ, against
     49) and the counters settle in fewer stages. Measured for the clk
     sampler and every capture slot (2-4 slots) at every corner, counters
     are safe in phases [9, 2n + 4) after their edge: L = 9, E = 4, which
     leaves >= 6.2 stages on both sides (>= 5.0 with flat wires). At a
     write, the slot selects change >= 476 ps after the hold time;
   - torn records: a slot's data muxes turn to the fresh values ~1.1 ns
     (typ) / ~1.9 ns (slow) after the previous write to its bank (pointer
     clock-to-Q, two select XORs, buffer, mux). A second write to the bank
     sooner (three merged toggles within that time) clocks the slot while
     its D pins are changing: bits metastable, some keeping the slot's old
     record. That was the gate's unflagged error before the record tags
     (slow, four edges within 2.7 ns: Gray bit 2 of counter B kept its value
     from the slot's record 1.9 us earlier, period count 376 for 320, right
     pin and level, a forward step the order check accepts), and the flat
     wire one at typ (same-bank writes 1.10 ns apart). The tags catch it:
     their D changes after every other D of the slot by more than setup +
     hold, at every select change, on and off (below). The sweep below
     flags every same-bank pair up to ~2.7 ns at slow (the tag's longer
     deselect also flags the clean record before the second write);
   - accounting against Capture_oracle's contract (per-pin flags by the end
     of the next 10-cycle quiet stretch; fault, overflow and runt windows;
     flagged events cover only their pins; every resync after a loss must
     mark the next event [lost]): in the gate (105k edges, a 400 ns quiet
     stretch every 50 edges) no unflagged error at any corner, with 1k or
     flat wires; every resync's next event marked; torn flags ~1 per
     270-1500 edges (slow-fast), nearly all on records that decoded right
     (conservative); the plausibility check fired only on events also
     marked overwritten (0 false flags); unflagged events' ages 1.0-6.1
     cycles against its 1-12 window;
   - timing, separately: 87-89% of edges validated against their true time
     within 2 ns; single-pin events, calibration fitted to a separate run
     and frozen: 0.6 bin rms (30/47/73 ps fast/typ/slow; bins 50/77/123 ps
     mean), p99 72/117/183 ps, rare outliers beyond 3 bins (gate: 1 in
     ~90k, 0.43 ns at slow); events merging several pins: 169-421 ps rms,
     worst 0.9 ns (one time for edges up to a merge window apart);
   - per-pin flags come <= 8 cycles after the error (gate); resyncs (D-13)
     mark the next event [lost], levels are re-established silently (no
     untimed re-baseline event);
   - edge-to-event latency (gate, 105k edges): the event is on the outputs
     after the 5th rising clock edge at or after the pin edge (median, every
     corner); 5-6 for an edge with no other edge within 100 ns, except when
     the drain is re-aligning the bank order after a runt (9); p99 7 and max
     10 under the random load (backlog of near-coincident edges);
   - snapshot race (pin -> D before pin -> bank clock): >= 280/422/656 ps
     fast/typ/slow;
   - collisions: two pins within ~200-475 ps (fast-slow) cancel in the XOR /
     clock trunk inertia (reported untimed by the level check or as stale);
     up to 350/575/900 ps (fast/typ/slow) they merge into one event
     attributed to both pins; beyond that they are separate events;
   - narrow merged pulses that one bank's clock tree passes and the other's
     swallows (runts) occur for near-coincident edges; the drain re-aligns
     the bank order and counts them. Bursts of three or more edges within
     ~1 ns can leave the banks too far apart: a capture fault, flagged and
     resynchronised (~1 per 700-1600 random edges, against 0-1 per 2000
     with flat wires);
   - with 2 slots per bank, bursts make the conservative overwrite check
     fire (no real overwrite was seen in simulation); 3 slots removes it at
     these rates. *)

open! Base
open Stdio
open Hardcaml_clockless
module T = Capture_timing

(* Asserted for both samplers, every bank and slot, at every corner. *)
let min_margin_stages = 4.

let%expect_test "counter selection margins of the capture slots and the clk sampler (Liberty 1k wire load)" =
  let ok =
    List.for_all T.corners ~f:(fun (corner, path) ->
      let ms = T.sampler_margins ~path () in
      let w = (List.hd_exn ms).w in
      printf "%-4s %s wire load: ring %.0f ps, stage %.1f ps\n" corner
        (T.wire_load_name T.baseline_wire_load path)
        (Event_sim.to_ps (w.tr.(w.periods * 2 * T.n).time - w.tr.(0).time) /. Float.of_int w.periods)
        (Event_sim.to_ps (Float.iround_nearest_exn w.stage));
      List.iter ms ~f:(fun m -> printf "     %s\n" (T.pp_sampler_margin m));
      T.samplers_ok ~min_stages:min_margin_stages ms)
  in
  printf "every margin >= %.0f stages, wavefronts in order, handoffs after hold: %b\n"
    min_margin_stages ok;
  [%expect {|
    typ  Liberty 1k wire load: ring 2610 ps, stage 71.7 ps
         clk sampler    safe phases A [ 8, 34+5) B [ 9, 34+5) | s=15 margins A  6.6/ 8.1 B  7.2/ 8.8 stages
         bank r slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 8.0 B  7.3/ 9.0 stages
                        ring -> D 245..277 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 754 ps after hold, wavefront in order: true
         bank r slot 1  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.5/ 8.0 B  7.3/ 9.0 stages
                        ring -> D 243..277 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 754 ps after hold, wavefront in order: true
         bank f slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 8.0 B  7.3/ 9.0 stages
                        ring -> D 245..277 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 754 ps after hold, wavefront in order: true
         bank f slot 1  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.5/ 8.0 B  7.3/ 9.0 stages
                        ring -> D 243..277 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 754 ps after hold, wavefront in order: true
    slow Liberty 1k wire load: ring 4185 ps, stage 114.6 ps
         clk sampler    safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.5/ 7.9 B  7.2/ 8.8 stages
         bank r slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.7 B  7.4/ 8.9 stages
                        ring -> D 392..446 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 1178 ps after hold, wavefront in order: true
         bank r slot 1  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.7 B  7.4/ 8.9 stages
                        ring -> D 392..446 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 1178 ps after hold, wavefront in order: true
         bank f slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.7 B  7.4/ 8.9 stages
                        ring -> D 392..446 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 1178 ps after hold, wavefront in order: true
         bank f slot 1  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.7 B  7.4/ 8.9 stages
                        ring -> D 392..446 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 1178 ps after hold, wavefront in order: true
    fast Liberty 1k wire load: ring 1692 ps, stage 46.5 ps
         clk sampler    safe phases A [ 8, 34+5) B [ 9, 34+5) | s=15 margins A  6.5/ 8.1 B  7.1/ 9.2 stages
         bank r slot 0  safe phases A [ 8, 34+5) B [ 9, 34+5) | s=15 margins A  6.5/ 8.1 B  7.2/ 9.3 stages
                        ring -> D 155..180 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 511 ps after hold, wavefront in order: true
         bank r slot 1  safe phases A [ 8, 34+5) B [ 9, 34+5) | s=15 margins A  6.5/ 8.1 B  7.2/ 9.3 stages
                        ring -> D 156..180 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 511 ps after hold, wavefront in order: true
         bank f slot 0  safe phases A [ 8, 34+5) B [ 9, 34+5) | s=15 margins A  6.5/ 8.1 B  7.2/ 9.3 stages
                        ring -> D 155..180 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 511 ps after hold, wavefront in order: true
         bank f slot 1  safe phases A [ 8, 34+5) B [ 9, 34+5) | s=15 margins A  6.5/ 8.1 B  7.2/ 9.3 stages
                        ring -> D 156..180 ps, clock skew in the record 0 ps (+-0 between writes), select handoff 511 ps after hold, wavefront in order: true
    every margin >= 4 stages, wavefronts in order, handoffs after hold: true
    |}]
;;

let%expect_test "record tags lag every other bit of their slot by more than the aperture (torn-record check)" =
  (* Pin_capture's torn-record check assumes that at every select change
     of a slot, on and off, each tag bit's D changes after every other bit's
     D by more than setup + hold, in each flop's own clock frame. Measured
     at three corners, then with 3% delay variation and clock insertion
     with 30 ps skew per flop clock pin (4 seeds). *)
  let ok = ref true in
  List.iter T.corners ~f:(fun (corner, path) ->
    List.iter (T.tag_margins ~path ()) ~f:(fun m ->
      if Float.(m.on_ps <= 0. || m.off_ps <= 0.) then ok := false;
      printf "%-4s %s\n" corner (T.pp_tag_margin m)));
  let worst = ref Float.infinity in
  List.iter [ 1; 2; 3; 4 ] ~f:(fun seed ->
    List.iter T.corners ~f:(fun (_, path) ->
      let config =
        { (T.config ~seed ()) with
          variation_sigma = 0.03
        ; clock_insertion = 0.3
        ; clock_skew_sigma = 0.03
        ; reg_clock_to_q = Event_sim.of_ns 0.5
        }
      in
      List.iter (T.tag_margins ~config ~path ()) ~f:(fun m ->
        worst := Float.min !worst (Float.min m.on_ps m.off_ps))));
  if Float.(!worst <= 0.) then ok := false;
  printf "with variation and clock skew: smallest margin over setup + hold %.0f ps\nall positive: %b\n" !worst !ok;
  [%expect {|
    typ  bank r: 48 select changes; tag D after every data D by >= 306 ps (on) / 328 ps (off); margin over setup + hold >= 227 / 282 ps
    typ  bank f: 48 select changes; tag D after every data D by >= 236 ps (on) / 328 ps (off); margin over setup + hold >= 188 / 282 ps
    slow bank r: 48 select changes; tag D after every data D by >= 482 ps (on) / 525 ps (off); margin over setup + hold >= 339 / 448 ps
    slow bank f: 48 select changes; tag D after every data D by >= 482 ps (on) / 525 ps (off); margin over setup + hold >= 339 / 448 ps
    fast bank r: 48 select changes; tag D after every data D by >= 202 ps (on) / 221 ps (off); margin over setup + hold >= 152 / 190 ps
    fast bank f: 48 select changes; tag D after every data D by >= 187 ps (on) / 221 ps (off); margin over setup + hold >= 152 / 190 ps
    with variation and clock skew: smallest margin over setup + hold 59 ps
    all positive: true
    |}]
;;

let%expect_test "two writes to one bank closer than the select handoff: torn records are flagged (slow)" =
  (* Triples of edges on three pins, the first and third (same bank) [d]
     apart, 1.9 us between triples, so that a slot's old record is about
     that old: a record torn between old and new decodes about 1.9 us off,
     which the order check can read as a forward step (the gate's error).
     Calibration from a separate random run. Per [d] (6 triples each):
     events flagged torn, events whose decoded time matches no true edge of
     their pins (all must be flagged). With the tag and plausibility checks
     removed the same sweep leaves such events unflagged. *)
  let path = "data/sg13cmos5l_excerpt_slow_1p08V_125C.lib" in
  let calibration =
    let edges =
      T.random_edges ~min_same_pin_ns:5. ~quiet_every:50 ~seed:12 ~count:1000 ~start_ns:800.
        ~mean_gap_ns:60. ~near:0.1 ()
    in
    T.fit (T.simulate ~path ~edges ~cycles:((List.last_exn edges |> fst) / T.clock_period + 450) ()) ~edges
  in
  let rows, t, _ =
    T.burst_sweep ~spacing_ns:1900. ~calibration ~path ~ds_ps:(List.range ~stride:100 1400 3001)
      ~reps:6 ~seed:1 ()
  in
  print_endline (Capture_oracle.pp t.summary);
  List.iter rows ~f:(fun r ->
    printf "d %4d ps: torn %d, wrong time flagged %d, unflagged %d\n" r.d_ps r.torn r.wrong_flagged
      r.wrong_unflagged);
  [%expect {|
    edges 314: timed 63, untimed 0, lost+flagged 251, bad+flagged 181 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    d 1400 ps: torn 6, wrong time flagged 0, unflagged 0
    d 1500 ps: torn 6, wrong time flagged 0, unflagged 0
    d 1600 ps: torn 6, wrong time flagged 0, unflagged 0
    d 1700 ps: torn 6, wrong time flagged 0, unflagged 0
    d 1800 ps: torn 6, wrong time flagged 0, unflagged 0
    d 1900 ps: torn 6, wrong time flagged 0, unflagged 0
    d 2000 ps: torn 6, wrong time flagged 0, unflagged 0
    d 2100 ps: torn 5, wrong time flagged 0, unflagged 0
    d 2200 ps: torn 5, wrong time flagged 0, unflagged 0
    d 2300 ps: torn 5, wrong time flagged 0, unflagged 0
    d 2400 ps: torn 6, wrong time flagged 0, unflagged 0
    d 2500 ps: torn 6, wrong time flagged 0, unflagged 0
    d 2600 ps: torn 6, wrong time flagged 0, unflagged 0
    d 2700 ps: torn 6, wrong time flagged 0, unflagged 0
    d 2800 ps: torn 1, wrong time flagged 0, unflagged 0
    d 2900 ps: torn 0, wrong time flagged 0, unflagged 0
    d 3000 ps: torn 0, wrong time flagged 0, unflagged 0
    |}]
;;

let%expect_test "random edges at three corners: attribution, flags, calibrated time (Liberty 1k wire load)" =
  List.iter T.corners ~f:(fun (corner, path) ->
    let random seed count =
      let edges =
        T.random_edges ~min_same_pin_ns:5. ~quiet_every:50 ~seed ~count ~start_ns:800.
          ~mean_gap_ns:60. ~near:0.1 ()
      in
      edges, (List.last_exn edges |> fst) / T.clock_period + 450
    in
    (* Calibration fitted to a separate run, then frozen. *)
    let calibration =
      let edges, cycles = random 12 1000 in
      T.fit (T.simulate ~path ~edges ~cycles ()) ~edges
    in
    let edges, cycles = random 11 2000 in
    let race = ref None in
    let r =
      T.simulate ~path ~edges ~cycles ~setup:(fun sim -> race := Some (T.watch_race sim)) ()
    in
    let t = T.analyse ~calibration r ~edges in
    print_endline (T.pp_timing ~corner ~calibrated:"on a separate run" t);
    List.iter (List.take t.summary.errors 5) ~f:(printf "     %s\n");
    let all, isolated = T.latencies t ~edges in
    printf "     edge -> event latency, clock edges (first at or after the edge = 1): all %s\n" (T.pp_latencies all);
    printf "       no other edge within 100 ns: %s\n" (T.pp_latencies isolated);
    let m = T.race_margins ~ptr_resets:r.ptr_resets (Option.value_exn !race) ~edges |> List.map ~f:Event_sim.to_ps in
    printf
      "     snapshot race (pin->D before pin->CLK): %d isolated edges, min %.0f ps, median %.0f ps\n"
      (List.length m)
      (List.fold m ~init:Float.infinity ~f:Float.min)
      (List.nth_exn (List.sort m ~compare:Float.compare) (List.length m / 2));
    printf "     X on outputs: %d; status: %s\n" r.x_outputs
      (String.concat ~sep:" " (List.map r.status ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v))));
  [%expect {|
    typ  edges 2000: timed 1759, untimed 69, lost+flagged 172, bad+flagged 90 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated on a separate run: bins 23..161 ps), delay pin->capture -464 ps
         timing: 1759 of 2000 edges validated within 2 ns (88.0%), 0 timed attributions without an estimate
         single-pin events: 1733, rms 53.7 ps (0.70 bins), p99 135.4 ps, worst 168.0 ps (2.19 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 26 attributions, rms 266.2 ps, worst 493.0 ps
         per-pin flag latency: 6 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs after a loss 30: first event after it marked lost 30, NOT MARKED 0 (D-13)
         edge -> event latency, clock edges (first at or after the edge = 1): all 1733 events, min 5, median 5, p99 7, max 9; 5:1377 6:323 7:27 8:1 9:5
           no other edge within 100 ns: 51 events, min 5, median 5, p99 6, max 6; 5:48 6:3
         snapshot race (pin->D before pin->CLK): 1513 isolated edges, min 432 ps, median 501 ps
         X on outputs: 0; status: capture_fault=1 fault_count=4 overflow=1 overflow_count=37 lost_edge=15 lost_edge_count=6 level_mismatch=15 level_mismatch_count=41 merge_error=7 merge_error_count=1 empty_count=12 stale_count=48 runt_count=7 torn_count=2 implausible=1 implausible_count=10 resyncing=0
    slow edges 2000: timed 1739, untimed 91, lost+flagged 170, bad+flagged 86 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 4185 ps (mean bin 123.1 ps, calibrated on a separate run: bins 38..270 ps), delay pin->capture -667 ps
         timing: 1739 of 2000 edges validated within 2 ns (87.0%), 0 timed attributions without an estimate
         single-pin events: 1671, rms 80.7 ps (0.66 bins), p99 202.3 ps, worst 265.4 ps (2.16 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 68 attributions, rms 419.7 ps, worst 862.3 ps
         per-pin flag latency: 7 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs after a loss 30: first event after it marked lost 30, NOT MARKED 0 (D-13)
         edge -> event latency, clock edges (first at or after the edge = 1): all 1671 events, min 5, median 5, p99 7, max 9; 5:1272 6:368 7:29 8:1 9:1
           no other edge within 100 ns: 50 events, min 5, median 5, p99 6, max 6; 5:46 6:4
         snapshot race (pin->D before pin->CLK): 1514 isolated edges, min 671 ps, median 780 ps
         X on outputs: 0; status: capture_fault=1 fault_count=4 overflow=1 overflow_count=38 lost_edge=7 lost_edge_count=5 level_mismatch=15 level_mismatch_count=61 merge_error=15 merge_error_count=3 empty_count=33 stale_count=46 runt_count=2 torn_count=3 implausible=1 implausible_count=10 resyncing=0
    fast edges 2000: timed 1774, untimed 50, lost+flagged 176, bad+flagged 89 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 1692 ps (mean bin 49.8 ps, calibrated on a separate run: bins 22..108 ps), delay pin->capture -275 ps
         timing: 1774 of 2000 edges validated within 2 ns (88.7%), 0 timed attributions without an estimate
         single-pin events: 1752, rms 31.4 ps (0.63 bins), p99 78.0 ps, worst 96.8 ps (1.94 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 22 attributions, rms 155.2 ps, worst 296.0 ps
         per-pin flag latency: 4 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs after a loss 32: first event after it marked lost 32, NOT MARKED 0 (D-13)
         edge -> event latency, clock edges (first at or after the edge = 1): all 1752 events, min 5, median 5, p99 7, max 10; 5:1416 6:307 7:27 8:1 10:1
           no other edge within 100 ns: 51 events, min 5, median 5, p99 6, max 6; 5:49 6:2
         snapshot race (pin->D before pin->CLK): 1512 isolated edges, min 285 ps, median 333 ps
         X on outputs: 0; status: capture_fault=1 fault_count=3 overflow=1 overflow_count=39 lost_edge=6 lost_edge_count=3 level_mismatch=15 level_mismatch_count=28 merge_error=14 merge_error_count=1 empty_count=12 stale_count=39 runt_count=2 torn_count=1 implausible=1 implausible_count=11 resyncing=0
    |}]
;;

let%expect_test "collision window: two pins delta apart, at three corners (Liberty 1k wire load)" =
  let deltas_ps = List.init 25 ~f:(fun k -> 25 * k) @ List.init 14 ~f:(fun k -> 700 + (100 * k)) in
  List.iter T.corners ~f:(fun (corner, path) ->
    let table, t = T.collision_sweep ~path ~deltas_ps ~reps:6 ~seed:5 () in
    let merged_max =
      List.filter table ~f:(fun c -> c.merged > 0) |> List.map ~f:(fun c -> c.delta_ps)
      |> List.max_elt ~compare:Int.compare |> Option.value ~default:0
    in
    let all_separate_from =
      List.rev table
      |> List.take_while ~f:(fun c -> c.separate = c.merged + c.separate + c.flagged + c.other)
      |> List.last |> Option.value_map ~default:(-1) ~f:(fun c -> c.delta_ps)
    in
    printf "%-4s %s\n" corner (Capture_oracle.pp t.summary);
    printf "     merged into one event up to %d ps; always separate from %d ps\n" merged_max all_separate_from;
    printf "     delta(ps):merged/separate/flagged/other ";
    List.iter table ~f:(fun c ->
      printf "%d:%d/%d/%d/%d " c.delta_ps c.merged c.separate c.flagged c.other);
    printf "\n");
  [%expect {|
    typ  edges 468: timed 316, untimed 152, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         merged into one event up to 575 ps; always separate from 600 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:0/0/6/0 250:0/0/6/0 275:0/0/6/0 300:0/0/6/0 325:2/0/4/0 350:4/0/2/0 375:5/0/1/0 400:6/0/0/0 425:6/0/0/0 450:6/0/0/0 475:5/1/0/0 500:5/1/0/0 525:1/5/0/0 550:2/4/0/0 575:1/5/0/0 600:0/6/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    slow edges 468: timed 237, untimed 231, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         merged into one event up to 900 ps; always separate from 1000 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:0/0/6/0 250:0/0/6/0 275:0/0/6/0 300:0/0/6/0 325:0/0/6/0 350:0/0/6/0 375:0/0/6/0 400:0/0/6/0 425:0/0/6/0 450:0/0/6/0 475:1/0/5/0 500:1/0/5/0 525:3/0/3/0 550:5/0/1/0 575:5/0/1/0 600:6/0/0/0 700:6/0/0/0 800:4/2/0/0 900:1/5/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    fast edges 468: timed 372, untimed 96, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         merged into one event up to 350 ps; always separate from 375 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:1/0/5/0 225:4/0/2/0 250:6/0/0/0 275:6/0/0/0 300:5/1/0/0 325:4/2/0/0 350:1/5/0/0 375:0/6/0/0 400:0/6/0/0 425:0/6/0/0 450:0/6/0/0 475:0/6/0/0 500:0/6/0/0 525:0/6/0/0 550:0/6/0/0 575:0/6/0/0 600:0/6/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    |}]
;;

(* The other slot counts (one-hot decode of a 3- or 4-flop Johnson pointer)
   with the real cells, typical corner. *)
module type Slots = sig
  val slots : int
end

let%expect_test "slots = 3 and 4 per bank (random edges at typ, counter selection at three corners; Liberty 1k wire load)" =
  let run (module M : Slots) =
    let module T = Capture_timing.Make (M) in
    let edges =
      T.random_edges ~min_same_pin_ns:5. ~seed:11 ~count:2000 ~start_ns:800. ~mean_gap_ns:60.
        ~near:0.1 ()
    in
    let cycles = (List.last_exn edges |> fst) / T.clock_period + 450 in
    let r = T.simulate ~path:"data/sg13cmos5l_excerpt.lib" ~edges ~cycles () in
    let t = T.analyse r ~edges in
    printf "slots %d: %s\n" M.slots (T.pp_timing ~corner:"typ" t);
    List.iter Capture_timing.corners ~f:(fun (corner, path) ->
      let ms = T.sampler_margins ~path () in
      printf "     %-4s counter selection: %s; ok %b\n" corner (T.pp_margin_summary ms)
        (T.samplers_ok ~min_stages:min_margin_stages ms);
      List.iter (T.tag_margins ~path ()) ~f:(fun m -> printf "          tags: %s\n" (T.pp_tag_margin m)));
    printf "     status: %s\n"
      (String.concat ~sep:" " (List.map r.status ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v)))
  in
  run (module struct let slots = 3 end);
  run (module struct let slots = 4 end);
  [%expect {|
    slots 3: typ  edges 2000: timed 1899, untimed 77, lost+flagged 24, bad+flagged 11 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated on this run: bins 30..164 ps), delay pin->capture -476 ps
         timing: 1899 of 2000 edges validated within 2 ns (95.0%), 0 timed attributions without an estimate
         single-pin events: 1863, rms 47.4 ps (0.62 bins), p99 117.4 ps, worst 155.7 ps (2.03 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 36 attributions, rms 305.2 ps, worst 518.3 ps
         per-pin flag latency: 8 errors covered, max 5 cycles (deadline: end of the first 12-cycle quiet stretch)
         resyncs after a loss 2: first event after it marked lost 2, NOT MARKED 0 (D-13)
         typ  counter selection: 7 samplers: safe phases A [9, 34+5) B [9, 34+4), s=15, smallest margin 6.2 stages; select handoff >= 750 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
              tags: bank r: 48 select changes; tag D after every data D by >= 176 ps (on) / 218 ps (off); margin over setup + hold >= 128 / 139 ps
              tags: bank f: 48 select changes; tag D after every data D by >= 171 ps (on) / 218 ps (off); margin over setup + hold >= 120 / 139 ps
         slow counter selection: 7 samplers: safe phases A [8, 34+5) B [9, 34+4), s=15, smallest margin 6.2 stages; select handoff >= 1170 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
              tags: bank r: 48 select changes; tag D after every data D by >= 263 ps (on) / 345 ps (off); margin over setup + hold >= 214 / 194 ps
              tags: bank f: 48 select changes; tag D after every data D by >= 204 ps (on) / 345 ps (off); margin over setup + hold >= 125 / 194 ps
         fast counter selection: 7 samplers: safe phases A [9, 34+5) B [9, 34+4), s=15, smallest margin 6.3 stages; select handoff >= 509 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
              tags: bank r: 48 select changes; tag D after every data D by >= 113 ps (on) / 144 ps (off); margin over setup + hold >= 97 / 91 ps
              tags: bank f: 48 select changes; tag D after every data D by >= 114 ps (on) / 144 ps (off); margin over setup + hold >= 82 / 91 ps
         status: capture_fault=1 fault_count=2 overflow=0 overflow_count=0 lost_edge=15 lost_edge_count=7 level_mismatch=15 level_mismatch_count=47 merge_error=13 merge_error_count=1 empty_count=18 stale_count=38 runt_count=6 torn_count=1 implausible=0 implausible_count=0 resyncing=0
    slots 4: typ  edges 2000: timed 1899, untimed 86, lost+flagged 15, bad+flagged 7 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated on this run: bins 24..170 ps), delay pin->capture -506 ps
         timing: 1899 of 2000 edges validated within 2 ns (95.0%), 0 timed attributions without an estimate
         single-pin events: 1855, rms 48.5 ps (0.63 bins), p99 123.3 ps, worst 155.6 ps (2.03 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 44 attributions, rms 324.3 ps, worst 584.0 ps
         per-pin flag latency: 8 errors covered, max 5 cycles (deadline: end of the first 14-cycle quiet stretch)
         resyncs after a loss 2: first event after it marked lost 2, NOT MARKED 0 (D-13)
         typ  counter selection: 9 samplers: safe phases A [9, 34+5) B [9, 34+4), s=15, smallest margin 6.0 stages; select handoff >= 753 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
              tags: bank r: 48 select changes; tag D after every data D by >= 183 ps (on) / 234 ps (off); margin over setup + hold >= 137 / 164 ps
              tags: bank f: 48 select changes; tag D after every data D by >= 152 ps (on) / 234 ps (off); margin over setup + hold >= 101 / 164 ps
         slow counter selection: 9 samplers: safe phases A [8, 34+5) B [9, 34+4), s=15, smallest margin 6.0 stages; select handoff >= 1175 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
              tags: bank r: 48 select changes; tag D after every data D by >= 269 ps (on) / 372 ps (off); margin over setup + hold >= 192 / 234 ps
              tags: bank f: 48 select changes; tag D after every data D by >= 260 ps (on) / 372 ps (off); margin over setup + hold >= 182 / 234 ps
         fast counter selection: 9 samplers: safe phases A [9, 34+5) B [9, 34+4), s=15, smallest margin 6.1 stages; select handoff >= 509 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
              tags: bank r: 48 select changes; tag D after every data D by >= 141 ps (on) / 156 ps (off); margin over setup + hold >= 108 / 108 ps
              tags: bank f: 48 select changes; tag D after every data D by >= 112 ps (on) / 156 ps (off); margin over setup + hold >= 98 / 108 ps
         status: capture_fault=1 fault_count=2 overflow=0 overflow_count=0 lost_edge=15 lost_edge_count=7 level_mismatch=15 level_mismatch_count=53 merge_error=0 merge_error_count=0 empty_count=24 stale_count=39 runt_count=3 torn_count=0 implausible=0 implausible_count=0 resyncing=0
    |}]
;;

let%expect_test "pins high at reset: no false lost_edge with the real edge counters (typ, Liberty 1k wire load)" =
  (* Pins 0 and 2 start high; each pin toggles 3 times, 200 ns apart. *)
  let initial = 0b0101 in
  let edges = List.init 12 ~f:(fun k -> Event_sim.of_ns 1000. + (k * Event_sim.of_ns 200.), k % 4) in
  let cycles = (List.last_exn edges |> fst) / T.clock_period + 50 in
  let r = T.simulate ~initial ~path:"data/sg13cmos5l_excerpt.lib" ~edges ~cycles () in
  List.iter r.events ~f:(fun e ->
    printf "cycle %3d: %s pins %x changed %x\n" e.cycle (if e.untimed then "untimed" else "timed  ") e.pins
      e.changed);
  printf "pin flags: %d; status: %s\n" (List.length r.flags.pin)
    (String.concat ~sep:" " (List.map r.status ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v)));
  [%expect {|
    cycle  55: timed   pins 4 changed 1
    cycle  65: timed   pins 6 changed 2
    cycle  75: timed   pins 2 changed 4
    cycle  85: timed   pins a changed 8
    cycle  95: timed   pins b changed 1
    cycle 105: timed   pins 9 changed 2
    cycle 115: timed   pins d changed 4
    cycle 125: timed   pins 5 changed 8
    cycle 135: timed   pins 4 changed 1
    cycle 145: timed   pins 6 changed 2
    cycle 155: timed   pins 2 changed 4
    cycle 165: timed   pins a changed 8
    pin flags: 0; status: capture_fault=0 fault_count=0 overflow=0 overflow_count=0 lost_edge=0 lost_edge_count=0 level_mismatch=0 level_mismatch_count=0 merge_error=0 merge_error_count=0 empty_count=0 stale_count=0 runt_count=0 torn_count=0 implausible=0 implausible_count=0 resyncing=0
    |}]
;;

let%expect_test "regression, flat 1 fF wires: random edges and counter selection at three corners" =
  let wire_load : Event_sim.Config.wire_load = Flat in
  List.iter T.corners ~f:(fun (corner, path) ->
    let edges = T.random_edges ~min_same_pin_ns:5. ~seed:11 ~count:2000 ~start_ns:800. ~mean_gap_ns:60. ~near:0.1 () in
    let cycles = (List.last_exn edges |> fst) / T.clock_period + 450 in
    let r = T.simulate ~wire_load ~path ~edges ~cycles () in
    let t = T.analyse r ~edges in
    print_endline (T.pp_timing ~corner t);
    List.iter (List.take t.summary.errors 5) ~f:(printf "     %s\n");
    let ms = T.sampler_margins ~wire_load ~path () in
    printf "     counter selection: %s; ok %b\n" (T.pp_margin_summary ms)
      (T.samplers_ok ~min_stages:min_margin_stages ms));
  [%expect {|
    typ  edges 2000: timed 1765, untimed 47, lost+flagged 188, bad+flagged 90 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 1598 ps (mean bin 47.0 ps, calibrated on this run: bins 26..101 ps), delay pin->capture -268 ps
         timing: 1765 of 2000 edges validated within 2 ns (88.2%), 0 timed attributions without an estimate
         single-pin events: 1747, rms 37.8 ps (0.80 bins), p99 92.0 ps, worst 120.0 ps (2.55 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 18 attributions, rms 163.4 ps, worst 267.0 ps
         per-pin flag latency: 6 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs after a loss 33: first event after it marked lost 33, NOT MARKED 0 (D-13)
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+5), s=15, smallest margin 5.0 stages; select handoff >= 603 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    slow edges 2000: timed 1773, untimed 69, lost+flagged 158, bad+flagged 70 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2542 ps (mean bin 74.8 ps, calibrated on this run: bins 52..148 ps), delay pin->capture -427 ps
         timing: 1773 of 2000 edges validated within 2 ns (88.7%), 0 timed attributions without an estimate
         single-pin events: 1737, rms 61.1 ps (0.82 bins), p99 146.2 ps, worst 183.2 ps (2.45 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 36 attributions, rms 329.7 ps, worst 765.7 ps
         per-pin flag latency: 6 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs after a loss 27: first event after it marked lost 27, NOT MARKED 0 (D-13)
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+5), s=15, smallest margin 5.0 stages; select handoff >= 929 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    fast edges 2000: timed 1771, untimed 34, lost+flagged 195, bad+flagged 96 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 1068 ps (mean bin 31.4 ps, calibrated on this run: bins 20..77 ps), delay pin->capture -188 ps
         timing: 1771 of 2000 edges validated within 2 ns (88.5%), 0 timed attributions without an estimate
         single-pin events: 1757, rms 23.4 ps (0.75 bins), p99 57.7 ps, worst 77.9 ps (2.48 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 14 attributions, rms 113.3 ps, worst 234.5 ps
         per-pin flag latency: 4 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs after a loss 35: first event after it marked lost 35, NOT MARKED 0 (D-13)
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+5), s=15, smallest margin 5.1 stages; select handoff >= 411 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    |}]
;;
