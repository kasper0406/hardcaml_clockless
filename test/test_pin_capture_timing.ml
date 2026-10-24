(* Pin_capture under Event_sim with Liberty delays at three corners: the real
   front (hand-placed NAND/XOR/clock trees, flop-cell slots and pointers),
   a fixed 17-stage ring with flop-cell counters, flop-cell synchroniser
   first stages, drained at 50 MHz. Expectations come from the true pin
   edges (Capture_oracle), not from the design. The long gate run is
   test/gate/capture_gate.exe. *)

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
