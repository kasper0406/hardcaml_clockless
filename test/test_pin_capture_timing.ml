(* Pin_capture under Event_sim with Liberty delays at three corners: the real
   front (hand-placed NAND/XOR/clock trees, flop-cell slots and pointers),
   a fixed 17-stage ring with flop-cell counters, flop-cell synchroniser
   first stages, drained at 50 MHz. Expectations come from the true pin
   edges (Capture_oracle), not from the design. The long gate run is
   test/gate/capture_gate.exe.

   Findings recorded by these tests (and the 105k-edge gate run):
   - zero unflagged errors: every edge is reported with the right pin and
     level, or covered by a flag;
   - single-edge error after code-density calibration (from the captures'
     own phases) and per (pin, polarity, bank) offsets: ~0.8 bin rms,
     <= ~2.8 bins worst at every corner (bins 31/47/75 ps fast/typ/slow);
   - snapshot race (pin -> D before pin -> bank clock): >= 190/284/437 ps
     fast/typ/slow, against a 40-110 ps setup time;
   - collisions: two pins within ~125-300 ps cancel in the XOR / clock
     trunk inertia (reported untimed by the level check or as stale);
     up to 250/350/600 ps (fast/typ/slow) they merge into one event
     attributed to both pins; beyond that they are separate events;
   - narrow merged pulses that one bank's clock tree passes and the other's
     swallows (runts) occur for near-coincident edges; the drain re-aligns
     the bank order and counts them;
   - rare single-edge outliers beyond 3 bins (~1 in 3000; worst 0.79 ns at
     fast) come from bursts of 3-4 edges on different pins within ~1 ns
     (runts, out-of-order bank records); a per-pin flag follows within a
     cycle, but the event itself is not flagged;
   - with 2 slots per bank, bursts make the conservative overwrite check
     fire (no real overwrite was seen in simulation); 3 slots removes it at
     these rates. *)

open! Base
open Stdio
open Hardcaml_clockless
module T = Capture_timing

let%expect_test "random edges at three corners: attribution, flags, calibrated time" =
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
    typ  edges 2000: timed 1798, untimed 49, lost+flagged 153, bad+flagged 134 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 1598 ps (mean bin 47.0 ps, calibrated bins 31..106 ps), delay pin->capture -1876 ps
         single-edge error: 1772 events, rms 50.4 ps (1.07 bins), worst 149.2 ps (3.18 bins); per-class offsets: rms 37.6 ps (0.80 bins), worst 127.1 ps (2.70 bins); >3 bins: 3
         snapshot race (pin->D before pin->CLK): 1528 isolated edges, min 284 ps, median 344 ps
         X on outputs: 0; status: capture_fault=1 fault_count=1 overflow=1 overflow_count=37 lost_edge=15 lost_edge_count=11 level_mismatch=15 level_mismatch_count=35 merge_error=14 merge_error_count=1 empty_count=12 stale_count=57 runt_count=1 resyncing=0
    slow edges 2000: timed 1803, untimed 71, lost+flagged 126, bad+flagged 99 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 2542 ps (mean bin 74.8 ps, calibrated bins 50..142 ps), delay pin->capture -431 ps
         single-edge error: 1761 events, rms 81.1 ps (1.08 bins), worst 236.7 ps (3.17 bins); per-class offsets: rms 61.5 ps (0.82 bins), worst 191.7 ps (2.56 bins); >3 bins: 2
         snapshot race (pin->D before pin->CLK): 1528 isolated edges, min 437 ps, median 524 ps
         X on outputs: 0; status: capture_fault=1 fault_count=1 overflow=1 overflow_count=31 lost_edge=7 lost_edge_count=7 level_mismatch=15 level_mismatch_count=49 merge_error=15 merge_error_count=5 empty_count=16 stale_count=47 runt_count=2 resyncing=0
    fast edges 2000: timed 1806, untimed 34, lost+flagged 160, bad+flagged 150 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 1068 ps (mean bin 31.4 ps, calibrated bins 21..72 ps), delay pin->capture -2326 ps
         single-edge error: 1790 events, rms 31.0 ps (0.99 bins), worst 101.1 ps (3.22 bins); per-class offsets: rms 24.0 ps (0.77 bins), worst 70.7 ps (2.25 bins); >3 bins: 1
         snapshot race (pin->D before pin->CLK): 1528 isolated edges, min 190 ps, median 228 ps
         X on outputs: 0; status: capture_fault=0 fault_count=0 overflow=1 overflow_count=42 lost_edge=15 lost_edge_count=12 level_mismatch=15 level_mismatch_count=35 merge_error=0 merge_error_count=0 empty_count=8 stale_count=52 runt_count=1 resyncing=0
    |}]
;;

let%expect_test "collision window: two pins delta apart, at three corners" =
  let deltas_ps = List.init 25 ~f:(fun k -> 25 * k) @ List.init 14 ~f:(fun k -> 700 + (100 * k)) in
  List.iter T.corners ~f:(fun (corner, path) ->
    let table, t = T.collision_sweep ~path ~deltas_ps ~reps:6 ~seed:5 in
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
    typ  edges 468: timed 368, untimed 100, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
         merged into one event up to 350 ps; always separate from 375 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:1/0/5/0 225:3/0/3/0 250:4/1/1/0 275:6/0/0/0 300:5/1/0/0 325:2/4/0/0 350:2/4/0/0 375:0/6/0/0 400:0/6/0/0 425:0/6/0/0 450:0/6/0/0 475:0/6/0/0 500:0/6/0/0 525:0/6/0/0 550:0/6/0/0 575:0/6/0/0 600:0/6/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    slow edges 468: timed 314, untimed 154, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
         merged into one event up to 600 ps; always separate from 700 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:0/0/6/0 150:0/0/6/0 175:0/0/6/0 200:0/0/6/0 225:0/0/6/0 250:0/0/6/0 275:0/0/6/0 300:1/0/5/0 325:3/0/3/0 350:4/0/2/0 375:5/0/1/0 400:5/0/1/0 425:6/0/0/0 450:5/1/0/0 475:5/1/0/0 500:2/4/0/0 525:3/3/0/0 550:1/5/0/0 575:1/5/0/0 600:1/5/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    fast edges 468: timed 400, untimed 68, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
         merged into one event up to 250 ps; always separate from 275 ps
         delta(ps):merged/separate/flagged/other 0:0/0/6/0 25:0/0/6/0 50:0/0/6/0 75:0/0/6/0 100:0/0/6/0 125:1/0/5/0 150:5/0/1/0 175:5/1/0/0 200:4/2/0/0 225:2/4/0/0 250:2/4/0/0 275:0/6/0/0 300:0/6/0/0 325:0/6/0/0 350:0/6/0/0 375:0/6/0/0 400:0/6/0/0 425:0/6/0/0 450:0/6/0/0 475:0/6/0/0 500:0/6/0/0 525:0/6/0/0 550:0/6/0/0 575:0/6/0/0 600:0/6/0/0 700:0/6/0/0 800:0/6/0/0 900:0/6/0/0 1000:0/6/0/0 1100:0/6/0/0 1200:0/6/0/0 1300:0/6/0/0 1400:0/6/0/0 1500:0/6/0/0 1600:0/6/0/0 1700:0/6/0/0 1800:0/6/0/0 1900:0/6/0/0 2000:0/6/0/0
    |}]
;;

(* The other slot counts (one-hot decode of a 3- or 4-flop Johnson pointer)
   with the real cells, typical corner. *)
module type Slots = sig
  val slots : int
end

let%expect_test "slots = 3 and 4 per bank (typ)" =
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
    printf "     status: %s\n"
      (String.concat ~sep:" " (List.map r.status ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v)))
  in
  run (module struct let slots = 3 end);
  run (module struct let slots = 4 end);
  [%expect {|
    slots 3: typ  edges 2000: timed 1933, untimed 56, lost+flagged 11, bad+flagged 18 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 1598 ps (mean bin 47.0 ps, calibrated bins 30..89 ps), delay pin->capture -1880 ps
         single-edge error: 1901 events, rms 49.6 ps (1.06 bins), worst 152.1 ps (3.24 bins); per-class offsets: rms 39.5 ps (0.84 bins), worst 110.4 ps (2.35 bins); >3 bins: 1
         status: capture_fault=1 fault_count=1 overflow=0 overflow_count=0 lost_edge=15 lost_edge_count=13 level_mismatch=15 level_mismatch_count=31 merge_error=14 merge_error_count=1 empty_count=14 stale_count=41 runt_count=3 resyncing=0
    slots 4: typ  edges 2000: timed 1933, untimed 58, lost+flagged 9, bad+flagged 13 | UNFLAGGED: lost 0, phantom 0 | pending 0
         ring 1598 ps (mean bin 47.0 ps, calibrated bins 32..94 ps), delay pin->capture -1892 ps
         single-edge error: 1903 events, rms 50.6 ps (1.08 bins), worst 153.4 ps (3.26 bins); per-class offsets: rms 40.7 ps (0.87 bins), worst 131.0 ps (2.79 bins); >3 bins: 3
         status: capture_fault=0 fault_count=0 overflow=0 overflow_count=0 lost_edge=15 lost_edge_count=10 level_mismatch=15 level_mismatch_count=34 merge_error=0 merge_error_count=0 empty_count=13 stale_count=35 runt_count=2 resyncing=0
    |}]
;;

let%expect_test "pins high at reset: no false lost_edge with the real edge counters (typ)" =
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
