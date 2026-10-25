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
   - zero unflagged errors: every edge is reported with the right pin and
     level, or covered by a flag;
   - single-edge error after code-density calibration (from the captures'
     own phases, on the same run) and per (pin, polarity, bank) offsets
     (held out): ~0.6 bin rms (30/47/76 ps fast/typ/slow; bins 50/77/123 ps
     mean), <= 2.2 bins worst except rare multi-pin-burst outliers beyond 3
     bins (gate: none at typ and fast, 3 in 30k at slow, worst 0.74 ns);
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
    let edges = T.random_edges ~min_same_pin_ns:5. ~seed:11 ~count:2000 ~start_ns:800. ~mean_gap_ns:60. ~near:0.1 () in
    let cycles = (List.last_exn edges |> fst) / T.clock_period + 450 in
    let race = ref None in
    let r =
      T.simulate ~path ~edges ~cycles ~setup:(fun sim -> race := Some (T.watch_race sim)) ()
    in
    let t = T.analyse r ~edges in
    print_endline (T.pp_timing ~corner t);
    List.iter (List.take t.summary.errors 5) ~f:(printf "     %s\n");
    let m = T.race_margins (Option.value_exn !race) ~edges |> List.map ~f:Event_sim.to_ps in
    printf
      "     snapshot race (pin->D before pin->CLK): %d isolated edges, min %.0f ps, median %.0f ps\n"
      (List.length m)
      (List.fold m ~init:Float.infinity ~f:Float.min)
      (List.nth_exn (List.sort m ~compare:Float.compare) (List.length m / 2));
    printf "     X on outputs: %d; status: %s\n" r.x_outputs
      (String.concat ~sep:" " (List.map r.status ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v))));
  [%expect {|
    typ  edges 2000: timed 1777, untimed 77, lost+flagged 146, bad+flagged 124 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated bins 29..162 ps), delay pin->capture -451 ps
         single-edge error: 1751 events, rms 60.6 ps (0.79 bins), worst 187.0 ps (2.44 bins); per-class offsets: rms 47.9 ps (0.62 bins), worst 151.3 ps (1.97 bins); >3 bins: 0
         snapshot race (pin->D before pin->CLK): 1528 isolated edges, min 422 ps, median 496 ps
         X on outputs: 0; status: capture_fault=1 fault_count=3 overflow=1 overflow_count=34 lost_edge=15 lost_edge_count=13 level_mismatch=15 level_mismatch_count=53 merge_error=13 merge_error_count=1 empty_count=15 stale_count=60 runt_count=6 resyncing=0
    slow edges 2000: timed 1766, untimed 101, lost+flagged 133, bad+flagged 113 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 4185 ps (mean bin 123.1 ps, calibrated bins 41..254 ps), delay pin->capture -635 ps
         single-edge error: 1702 events, rms 97.6 ps (0.79 bins), worst 311.2 ps (2.53 bins); per-class offsets: rms 74.5 ps (0.61 bins), worst 223.3 ps (1.81 bins); >3 bins: 0
         snapshot race (pin->D before pin->CLK): 1528 isolated edges, min 656 ps, median 774 ps
         X on outputs: 0; status: capture_fault=1 fault_count=3 overflow=1 overflow_count=32 lost_edge=7 lost_edge_count=9 level_mismatch=15 level_mismatch_count=64 merge_error=15 merge_error_count=3 empty_count=31 stale_count=71 runt_count=3 resyncing=0
    fast edges 2000: timed 1796, untimed 52, lost+flagged 152, bad+flagged 134 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 1692 ps (mean bin 49.8 ps, calibrated bins 19..91 ps), delay pin->capture -1998 ps
         single-edge error: 1772 events, rms 37.8 ps (0.76 bins), worst 197.4 ps (3.97 bins); per-class offsets: rms 31.0 ps (0.62 bins), worst 206.0 ps (4.14 bins); >3 bins: 1
         snapshot race (pin->D before pin->CLK): 1528 isolated edges, min 280 ps, median 330 ps
         X on outputs: 0; status: capture_fault=1 fault_count=3 overflow=1 overflow_count=36 lost_edge=14 lost_edge_count=9 level_mismatch=15 level_mismatch_count=34 merge_error=14 merge_error_count=1 empty_count=14 stale_count=63 runt_count=3 resyncing=0
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
    typ  edges 468: timed 315, untimed 153, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
         merged into one event up to 575 ps; always separate from 600 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:0/0/6/0 250:0/0/6/0 275:0/0/6/0 300:0/0/6/0 325:2/0/4/0 350:4/0/2/0 375:5/0/1/0 400:6/0/0/0 425:6/0/0/0 450:5/1/0/0 475:5/1/0/0 500:4/2/0/0 525:2/4/0/0 550:0/6/0/0 575:2/4/0/0 600:0/6/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    slow edges 468: timed 225, untimed 243, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
         merged into one event up to 900 ps; always separate from 1000 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:0/0/6/0 250:0/0/6/0 275:0/0/6/0 300:0/0/6/0 325:0/0/6/0 350:0/0/6/0 375:0/0/6/0 400:0/0/6/0 425:0/0/6/0 450:0/0/6/0 475:0/0/6/0 500:1/0/5/0 525:3/0/3/0 550:5/0/1/0 575:5/0/1/0 600:5/0/1/0 700:5/1/0/0 800:4/2/0/0 900:1/5/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    fast edges 468: timed 367, untimed 101, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
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
    slots 3: typ  edges 2000: timed 1904, untimed 79, lost+flagged 17, bad+flagged 12 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated bins 33..187 ps), delay pin->capture -458 ps
         single-edge error: 1864 events, rms 64.3 ps (0.84 bins), worst 182.1 ps (2.37 bins); per-class offsets: rms 49.3 ps (0.64 bins), worst 155.2 ps (2.02 bins); >3 bins: 0
         typ  counter selection: 7 samplers: safe phases A [8, 34+4) B [9, 34+4), s=15, smallest margin 6.4 stages; select handoff >= 701 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         slow counter selection: 7 samplers: safe phases A [8, 34+4) B [9, 34+4), s=15, smallest margin 6.5 stages; select handoff >= 1089 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         fast counter selection: 7 samplers: safe phases A [8, 34+5) B [9, 34+4), s=15, smallest margin 6.5 stages; select handoff >= 476 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
         status: capture_fault=1 fault_count=2 overflow=0 overflow_count=0 lost_edge=15 lost_edge_count=8 level_mismatch=15 level_mismatch_count=48 merge_error=15 merge_error_count=2 empty_count=21 stale_count=41 runt_count=6 resyncing=0
    slots 4: typ  edges 2000: timed 1896, untimed 83, lost+flagged 21, bad+flagged 13 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 2610 ps (mean bin 76.8 ps, calibrated bins 26..167 ps), delay pin->capture -490 ps
         single-edge error: 1858 events, rms 62.2 ps (0.81 bins), worst 192.7 ps (2.51 bins); per-class offsets: rms 47.5 ps (0.62 bins), worst 153.8 ps (2.00 bins); >3 bins: 0
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
  printf "pin flags: %d; status: %s\n" (List.length r.pin_flags)
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
    typ  edges 2000: timed 1798, untimed 49, lost+flagged 153, bad+flagged 134 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 1598 ps (mean bin 47.0 ps, calibrated bins 31..106 ps), delay pin->capture -1876 ps
         single-edge error: 1772 events, rms 50.4 ps (1.07 bins), worst 149.2 ps (3.18 bins); per-class offsets: rms 37.6 ps (0.80 bins), worst 127.1 ps (2.70 bins); >3 bins: 3
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+5), s=15, smallest margin 5.2 stages; select handoff >= 595 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    slow edges 2000: timed 1803, untimed 71, lost+flagged 126, bad+flagged 99 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 2542 ps (mean bin 74.8 ps, calibrated bins 50..142 ps), delay pin->capture -431 ps
         single-edge error: 1761 events, rms 81.1 ps (1.08 bins), worst 236.7 ps (3.17 bins); per-class offsets: rms 61.5 ps (0.82 bins), worst 191.7 ps (2.56 bins); >3 bins: 2
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+4), s=15, smallest margin 5.0 stages; select handoff >= 915 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    fast edges 2000: timed 1806, untimed 34, lost+flagged 160, bad+flagged 150 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 1068 ps (mean bin 31.4 ps, calibrated bins 21..72 ps), delay pin->capture -2326 ps
         single-edge error: 1790 events, rms 31.0 ps (0.99 bins), worst 101.1 ps (3.22 bins); per-class offsets: rms 24.0 ps (0.77 bins), worst 70.7 ps (2.25 bins); >3 bins: 1
         counter selection: 5 samplers: safe phases A [10, 34+5) B [9, 34+5), s=15, smallest margin 5.3 stages; select handoff >= 406 ps after hold, wavefronts in order: true, clock arrival +-0 ps between writes; ok true
    |}]
;;
