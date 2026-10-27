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
   - accounting against Capture_oracle's contract (per-pin flags by the end
     of the next 10-cycle quiet stretch; fault, overflow and runt windows;
     flagged events cover only their pins): in the gate (105k edges, a
     400 ns quiet stretch every 50 edges) one unflagged error, at slow: in a
     burst of four edges on four pins within 2.7 ns, pin 1's edge came out
     with the right pin and level but a time 1.9 us off (a capture record
     with a wrong period count; no flag on the event, the level and count
     checks pass, and the order check only catches backward steps). The old
     400-cycle flag slack had hidden it behind a later, unrelated flag. With
     flat wires the gate finds one more of the same kind (typ), and at fast
     a single-pin event one ring period off (1.04 ns): the 2 ns attribution
     tolerance exceeds the ring period there, so the timing report counts
     errors beyond half a ring period separately;
   - timing, separately: 89-91% of edges validated against their true time
     within 2 ns; single-pin events, calibration fitted to a separate run
     and frozen: 0.6 bin rms (30/47/74 ps fast/typ/slow; bins 50/77/123 ps
     mean), p99 72/116/188 ps, rare multi-pin-burst outliers beyond 3 bins
     (gate: 0/2/2 in ~30k, worst 0.84 ns at slow); events merging several
     pins: 168-408 ps rms, worst 1.1 ns (one time for edges up to a merge
     window apart);
   - per-pin flags come <= 8 cycles after the error (gate); overflow and
     fault resyncs (D-13): no event after a resync carries a loss mark and
     levels are re-established silently, not by an untimed event;
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
         clk sampler    safe phases A [ 8, 34+5) B [ 8, 34+4) | s=15 margins A  6.8/ 7.9 B  7.4/ 8.6 stages
         bank r slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.7 B  7.3/ 8.8 stages
                        ring -> D 245..277 ps, clock skew in the record 14 ps (+-0 between writes), select handoff 732 ps after hold, wavefront in order: true
         bank r slot 1  safe phases A [ 8, 34+4) B [ 9, 34+4) | s=15 margins A  6.9/ 7.6 B  7.7/ 8.6 stages
                        ring -> D 243..277 ps, clock skew in the record 14 ps (+-0 between writes), select handoff 732 ps after hold, wavefront in order: true
         bank f slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.7 B  7.3/ 8.8 stages
                        ring -> D 245..277 ps, clock skew in the record 14 ps (+-0 between writes), select handoff 732 ps after hold, wavefront in order: true
         bank f slot 1  safe phases A [ 8, 34+4) B [ 9, 34+4) | s=15 margins A  6.9/ 7.6 B  7.7/ 8.6 stages
                        ring -> D 243..277 ps, clock skew in the record 14 ps (+-0 between writes), select handoff 732 ps after hold, wavefront in order: true
    slow Liberty 1k wire load: ring 4185 ps, stage 114.6 ps
         clk sampler    safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.8/ 7.7 B  7.4/ 8.6 stages
         bank r slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.5 B  7.3/ 8.7 stages
                        ring -> D 392..446 ps, clock skew in the record 20 ps (+-0 between writes), select handoff 1141 ps after hold, wavefront in order: true
         bank r slot 1  safe phases A [ 8, 34+4) B [ 9, 34+4) | s=15 margins A  6.8/ 7.2 B  7.8/ 8.5 stages
                        ring -> D 392..446 ps, clock skew in the record 20 ps (+-0 between writes), select handoff 1141 ps after hold, wavefront in order: true
         bank f slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.4/ 7.5 B  7.3/ 8.7 stages
                        ring -> D 392..446 ps, clock skew in the record 20 ps (+-0 between writes), select handoff 1141 ps after hold, wavefront in order: true
         bank f slot 1  safe phases A [ 8, 34+4) B [ 9, 34+4) | s=15 margins A  6.8/ 7.2 B  7.8/ 8.5 stages
                        ring -> D 392..446 ps, clock skew in the record 20 ps (+-0 between writes), select handoff 1141 ps after hold, wavefront in order: true
    fast Liberty 1k wire load: ring 1692 ps, stage 46.5 ps
         clk sampler    safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.7/ 7.9 B  7.3/ 9.0 stages
         bank r slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.5/ 7.9 B  7.2/ 9.1 stages
                        ring -> D 155..180 ps, clock skew in the record 9 ps (+-0 between writes), select handoff 499 ps after hold, wavefront in order: true
         bank r slot 1  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.9/ 7.7 B  7.6/ 8.9 stages
                        ring -> D 156..180 ps, clock skew in the record 9 ps (+-0 between writes), select handoff 499 ps after hold, wavefront in order: true
         bank f slot 0  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.5/ 7.9 B  7.2/ 9.1 stages
                        ring -> D 155..180 ps, clock skew in the record 9 ps (+-0 between writes), select handoff 499 ps after hold, wavefront in order: true
         bank f slot 1  safe phases A [ 8, 34+5) B [ 9, 34+4) | s=15 margins A  6.9/ 7.7 B  7.6/ 8.9 stages
                        ring -> D 156..180 ps, clock skew in the record 9 ps (+-0 between writes), select handoff 499 ps after hold, wavefront in order: true
    every margin >= 4 stages, wavefronts in order, handoffs after hold: true
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
    let m = T.race_margins (Option.value_exn !race) ~edges |> List.map ~f:Event_sim.to_ps in
    printf
      "     snapshot race (pin->D before pin->CLK): %d isolated edges, min %.0f ps, median %.0f ps\n"
      (List.length m)
      (List.fold m ~init:Float.infinity ~f:Float.min)
      (List.nth_exn (List.sort m ~compare:Float.compare) (List.length m / 2));
    printf "     X on outputs: %d; status: %s\n" r.x_outputs
      (String.concat ~sep:" " (List.map r.status ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v))));
  [%expect {|
    typ  edges 2000: timed 1777, untimed 77, lost+flagged 146, bad+flagged 123 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated on a separate run: bins 25..180 ps), delay pin->capture -459 ps
         timing: 1777 of 2000 edges validated within 2 ns (88.8%), 0 timed attributions without an estimate
         single-pin events: 1755, rms 53.3 ps (0.69 bins), p99 134.7 ps, worst 178.5 ps (2.33 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 22 attributions, rms 267.5 ps, worst 537.4 ps
         per-pin flag latency: 8 errors covered, max 5 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs 27: first event after it loss-marked 0, levels re-baselined by an untimed event 1
         edge -> event latency, clock edges (first at or after the edge = 1): all 1755 events, min 5, median 5, p99 7, max 9; 5:1398 6:323 7:29 8:1 9:4
           no other edge within 100 ns: 51 events, min 5, median 5, p99 6, max 6; 5:49 6:2
         snapshot race (pin->D before pin->CLK): 1530 isolated edges, min 422 ps, median 496 ps
         X on outputs: 0; status: capture_fault=1 fault_count=3 overflow=1 overflow_count=34 lost_edge=15 lost_edge_count=13 level_mismatch=15 level_mismatch_count=53 merge_error=13 merge_error_count=1 empty_count=13 stale_count=60 runt_count=6 resyncing=0
    slow edges 2000: timed 1775, untimed 97, lost+flagged 128, bad+flagged 108 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 4185 ps (mean bin 123.1 ps, calibrated on a separate run: bins 46..241 ps), delay pin->capture -680 ps
         timing: 1775 of 2000 edges validated within 2 ns (88.8%), 0 timed attributions without an estimate
         single-pin events: 1707, rms 83.3 ps (0.68 bins), p99 204.1 ps, worst 261.9 ps (2.13 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 68 attributions, rms 410.4 ps, worst 843.1 ps
         per-pin flag latency: 11 errors covered, max 5 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs 24: first event after it loss-marked 0, levels re-baselined by an untimed event 1
         edge -> event latency, clock edges (first at or after the edge = 1): all 1707 events, min 5, median 5, p99 7, max 9; 5:1304 6:371 7:30 8:1 9:1
           no other edge within 100 ns: 51 events, min 5, median 5, p99 6, max 6; 5:45 6:6
         snapshot race (pin->D before pin->CLK): 1530 isolated edges, min 656 ps, median 774 ps
         X on outputs: 0; status: capture_fault=1 fault_count=2 overflow=1 overflow_count=32 lost_edge=7 lost_edge_count=8 level_mismatch=15 level_mismatch_count=61 merge_error=15 merge_error_count=2 empty_count=34 stale_count=68 runt_count=2 resyncing=0
    fast edges 2000: timed 1795, untimed 51, lost+flagged 154, bad+flagged 136 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 1692 ps (mean bin 49.8 ps, calibrated on a separate run: bins 21..109 ps), delay pin->capture -267 ps
         timing: 1795 of 2000 edges validated within 2 ns (89.8%), 0 timed attributions without an estimate
         single-pin events: 1773, rms 33.3 ps (0.67 bins), p99 82.7 ps, worst 99.0 ps (1.99 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 22 attributions, rms 162.6 ps, worst 286.8 ps
         per-pin flag latency: 5 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs 29: first event after it loss-marked 0, levels re-baselined by an untimed event 1
         edge -> event latency, clock edges (first at or after the edge = 1): all 1773 events, min 5, median 5, p99 7, max 10; 5:1439 6:305 7:27 8:1 10:1
           no other edge within 100 ns: 51 events, min 5, median 5, p99 6, max 6; 5:49 6:2
         snapshot race (pin->D before pin->CLK): 1530 isolated edges, min 280 ps, median 330 ps
         X on outputs: 0; status: capture_fault=1 fault_count=3 overflow=1 overflow_count=36 lost_edge=14 lost_edge_count=9 level_mismatch=15 level_mismatch_count=35 merge_error=14 merge_error_count=1 empty_count=12 stale_count=61 runt_count=2 resyncing=0
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
    typ  edges 468: timed 315, untimed 153, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         merged into one event up to 575 ps; always separate from 600 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:0/0/6/0 250:0/0/6/0 275:0/0/6/0 300:0/0/6/0 325:2/0/4/0 350:4/0/2/0 375:5/0/1/0 400:6/0/0/0 425:6/0/0/0 450:5/1/0/0 475:5/1/0/0 500:4/2/0/0 525:2/4/0/0 550:0/6/0/0 575:2/4/0/0 600:0/6/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    slow edges 468: timed 225, untimed 243, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         merged into one event up to 900 ps; always separate from 1000 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:0/0/6/0 250:0/0/6/0 275:0/0/6/0 300:0/0/6/0 325:0/0/6/0 350:0/0/6/0 375:0/0/6/0 400:0/0/6/0 425:0/0/6/0 450:0/0/6/0 475:0/0/6/0 500:1/0/5/0 525:3/0/3/0 550:5/0/1/0 575:5/0/1/0 600:5/0/1/0 700:5/1/0/0 800:4/2/0/0 900:1/5/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    fast edges 468: timed 367, untimed 101, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         merged into one event up to 350 ps; always separate from 375 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:3/0/3/0 250:5/0/1/0 275:6/0/0/0 300:5/1/0/0 325:3/3/0/0 350:1/5/0/0 375:0/6/0/0 400:0/6/0/0 425:0/6/0/0 450:0/6/0/0 475:0/6/0/0 500:0/6/0/0 525:0/6/0/0 550:0/6/0/0 575:0/6/0/0 600:0/6/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
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
        (T.samplers_ok ~min_stages:min_margin_stages ms));
    printf "     status: %s\n"
      (String.concat ~sep:" " (List.map r.status ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v)))
  in
  run (module struct let slots = 3 end);
  run (module struct let slots = 4 end);
  [%expect {|
    slots 3: typ  edges 2000: timed 1904, untimed 79, lost+flagged 17, bad+flagged 12 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated on this run: bins 33..187 ps), delay pin->capture -458 ps
         timing: 1904 of 2000 edges validated within 2 ns (95.2%), 0 timed attributions without an estimate
         single-pin events: 1864, rms 49.5 ps (0.64 bins), p99 124.0 ps, worst 164.2 ps (2.14 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 40 attributions, rms 296.0 ps, worst 534.7 ps
         per-pin flag latency: 8 errors covered, max 5 cycles (deadline: end of the first 12-cycle quiet stretch)
         resyncs 2: first event after it loss-marked 0, levels re-baselined by an untimed event 0
         typ  counter selection: 7 samplers: safe phases A [8, 34+4) B [9, 34+4), s=15, smallest margin 6.4 stages; select handoff >= 701 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         slow counter selection: 7 samplers: safe phases A [8, 34+4) B [9, 34+4), s=15, smallest margin 6.5 stages; select handoff >= 1089 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         fast counter selection: 7 samplers: safe phases A [8, 34+5) B [9, 34+4), s=15, smallest margin 6.5 stages; select handoff >= 476 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         status: capture_fault=1 fault_count=2 overflow=0 overflow_count=0 lost_edge=15 lost_edge_count=8 level_mismatch=15 level_mismatch_count=48 merge_error=15 merge_error_count=2 empty_count=21 stale_count=41 runt_count=6 resyncing=0
    slots 4: typ  edges 2000: timed 1896, untimed 83, lost+flagged 21, bad+flagged 13 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated on this run: bins 26..167 ps), delay pin->capture -490 ps
         timing: 1896 of 2000 edges validated within 2 ns (94.8%), 0 timed attributions without an estimate
         single-pin events: 1858, rms 48.4 ps (0.63 bins), p99 122.9 ps, worst 164.5 ps (2.14 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 38 attributions, rms 318.1 ps, worst 566.1 ps
         per-pin flag latency: 8 errors covered, max 5 cycles (deadline: end of the first 14-cycle quiet stretch)
         resyncs 3: first event after it loss-marked 0, levels re-baselined by an untimed event 0
         typ  counter selection: 9 samplers: safe phases A [8, 34+5) B [9, 34+4), s=15, smallest margin 6.2 stages; select handoff >= 701 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         slow counter selection: 9 samplers: safe phases A [8, 34+4) B [9, 34+4), s=15, smallest margin 6.2 stages; select handoff >= 1089 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         fast counter selection: 9 samplers: safe phases A [8, 34+5) B [9, 34+4), s=15, smallest margin 6.3 stages; select handoff >= 476 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         status: capture_fault=1 fault_count=3 overflow=0 overflow_count=0 lost_edge=15 lost_edge_count=8 level_mismatch=15 level_mismatch_count=52 merge_error=0 merge_error_count=0 empty_count=21 stale_count=41 runt_count=3 resyncing=0
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
    pin flags: 0; status: capture_fault=0 fault_count=0 overflow=0 overflow_count=0 lost_edge=0 lost_edge_count=0 level_mismatch=0 level_mismatch_count=0 merge_error=0 merge_error_count=0 empty_count=0 stale_count=0 runt_count=0 resyncing=0
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
    typ  edges 2000: timed 1798, untimed 49, lost+flagged 153, bad+flagged 134 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 1598 ps (mean bin 47.0 ps, calibrated on this run: bins 31..106 ps), delay pin->capture -278 ps
         timing: 1798 of 2000 edges validated within 2 ns (89.9%), 0 timed attributions without an estimate
         single-pin events: 1772, rms 38.1 ps (0.81 bins), p99 96.5 ps, worst 134.2 ps (2.86 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 26 attributions, rms 187.0 ps, worst 466.4 ps
         per-pin flag latency: 10 errors covered, max 5 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs 28: first event after it loss-marked 0, levels re-baselined by an untimed event 1
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+5), s=15, smallest margin 5.2 stages; select handoff >= 595 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    slow edges 2000: timed 1803, untimed 71, lost+flagged 126, bad+flagged 99 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 2542 ps (mean bin 74.8 ps, calibrated on this run: bins 50..142 ps), delay pin->capture -431 ps
         timing: 1803 of 2000 edges validated within 2 ns (90.2%), 0 timed attributions without an estimate
         single-pin events: 1761, rms 61.4 ps (0.82 bins), p99 153.1 ps, worst 192.6 ps (2.58 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 42 attributions, rms 334.4 ps, worst 772.9 ps
         per-pin flag latency: 8 errors covered, max 5 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs 23: first event after it loss-marked 0, levels re-baselined by an untimed event 1
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+4), s=15, smallest margin 5.0 stages; select handoff >= 915 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    fast edges 2000: timed 1806, untimed 34, lost+flagged 160, bad+flagged 150 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
         ring 1068 ps (mean bin 31.4 ps, calibrated on this run: bins 21..72 ps), delay pin->capture -190 ps
         timing: 1806 of 2000 edges validated within 2 ns (90.3%), 0 timed attributions without an estimate
         single-pin events: 1790, rms 23.9 ps (0.76 bins), p99 59.2 ps, worst 80.2 ps (2.55 bins), >3 bins 0, >0.5 ns 0, >half a ring period 0
         multi-pin events: 16 attributions, rms 118.4 ps, worst 202.9 ps
         per-pin flag latency: 4 errors covered, max 4 cycles (deadline: end of the first 10-cycle quiet stretch)
         resyncs 30: first event after it loss-marked 0, levels re-baselined by an untimed event 2
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+5), s=15, smallest margin 5.3 stages; select handoff >= 406 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    |}]
;;
