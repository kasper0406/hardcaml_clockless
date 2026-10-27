(* The oracle itself: it must reject what the coverage contract does not
   excuse (Capture_oracle). Synthetic inputs: cycle c is at time c * 1000. *)

open! Base
open Stdio
module O = Capture_oracle

let cycle_time c = c * 1000
let cycle_of_time t = (t + 999) / 1000
let windows = { O.in_flight = 4; back = 4; before = 8; after = 5; event_age = 12 }
let quiet_cycles = 10
let no_flags = { O.pin = []; faults = []; resync = []; runts = [] }

let event ?(untimed = false) ?(flagged = false) ?(loss_mark = false) ?est ~cycle ~pins ~changed () =
  { O.cycle; untimed; pins; changed; reconfig = 0; stale = 0; flagged; loss_mark; est }
;;

(* Edges are (cycle, pin), each a rise. *)
let check ?(flags = no_flags) ?(events = []) edges =
  let edges =
    List.map edges ~f:(fun (c, pin) -> { O.time = cycle_time c - 500; pin; level = true })
  in
  let end_cycle = 200 in
  let s =
    O.check ~pins:2 ~edges ~events ~flags ~cycle_of_time ~cycle_time ~match_tol:300 ~windows
      ~deadline:
        (O.deadlines ~quiet_cycles ~end_cycle
           (List.map edges ~f:(fun (e : O.edge) -> cycle_of_time e.time)))
      ~end_cycle
  in
  print_endline (O.pp s);
  List.iter s.errors ~f:(printf "  %s\n")
;;

let%expect_test "per-pin flags cover only their pin, before the deadline" =
  (* an edge on pin 0 at cycle 10, never reported *)
  check ~flags:{ no_flags with pin = [ 12, 0b10 ] } [ 10, 0 ];
  check ~flags:{ no_flags with pin = [ 12, 0b01 ] } [ 10, 0 ];
  (* the deadline: the end of the first 10-cycle quiet stretch, which
     starts after the edge's cycle: cycle 21 *)
  check ~flags:{ no_flags with pin = [ 21, 0b01 ] } [ 10, 0 ];
  check ~flags:{ no_flags with pin = [ 22, 0b01 ] } [ 10, 0 ];
  (* edges on pin 1 every 5 cycles until 65 push the deadline to 76 *)
  check
    ~flags:{ no_flags with pin = [ 65, 0b01 ] }
    ~events:
      (List.init 10 ~f:(fun k ->
         event ~cycle:(20 + (5 * k) + 6) ~pins:0b10 ~changed:0b10 ~est:(cycle_time (20 + (5 * k)) - 500) ()))
    ((10, 0) :: List.init 10 ~f:(fun k -> 20 + (5 * k), 1));
  [%expect {|
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 0 (late flag 0) | pending 0
      lost: pin 0 time 9500 (cycle 10) level true
    edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 0 (late flag 1) | pending 0
      lost (late flag): pin 0 time 9500 (cycle 10) level true
    edges 11: timed 10, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    |}]
;;

let%expect_test "faults cover only their window" =
  (* a fault at 12 with resync 13..20: edges captured from cycle 4 to 25 *)
  let flags = { no_flags with faults = [ 12 ]; resync = List.range 13 21 } in
  List.iter [ 3; 4; 25; 26; 40 ] ~f:(fun c -> check ~flags [ c, 1 ]);
  (* a runt at 12: edges from 4 to 17 *)
  let flags = { no_flags with runts = [ 12 ] } in
  List.iter [ 17; 18 ] ~f:(fun c -> check ~flags [ c, 1 ]);
  [%expect {|
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 0 (late flag 0) | pending 0
      lost: pin 1 time 2500 (cycle 3) level true
    edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 0 (late flag 0) | pending 0
      lost: pin 1 time 25500 (cycle 26) level true
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 0 (late flag 0) | pending 0
      lost: pin 1 time 39500 (cycle 40) level true
    edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 0 (late flag 0) | pending 0
      lost: pin 1 time 17500 (cycle 18) level true
    |}]
;;

let%expect_test "flagged events cover only the pins they attribute" =
  let events = [ event ~flagged:true ~cycle:18 ~pins:0b01 ~changed:0b01 ~est:9500 () ] in
  check ~events [ 10, 0 ];
  check ~events [ 10, 1 ];
  [%expect {|
    edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 1 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 1 | UNFLAGGED: lost 1, phantom 0 (late flag 0) | pending 0
      lost: pin 1 time 9500 (cycle 10) level true
    |}]
;;

let%expect_test "a timed attribution needs a time estimate that matches" =
  check ~events:[ event ~cycle:16 ~pins:0b01 ~changed:0b01 ~est:9500 () ] [ 10, 0 ];
  check ~events:[ event ~cycle:16 ~pins:0b01 ~changed:0b01 () ] [ 10, 0 ];
  check ~events:[ event ~cycle:16 ~pins:0b01 ~changed:0b01 ~est:8000 () ] [ 10, 0 ];
  [%expect {|
    edges 1: timed 1, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 1 (late flag 0) | pending 0
      phantom: cycle 16 pin 0 level true est -
      lost: pin 0 time 9500 (cycle 10) level true
    edges 1: timed 0, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 1, phantom 1 (late flag 0) | pending 0
      phantom: cycle 16 pin 0 level true est 8000
      lost: pin 0 time 9500 (cycle 10) level true
    |}]
;;

let%expect_test "a fault discards what is unread: no later timed report of an older edge" =
  (* edge at 10, fault at 12, resync to 20 *)
  let flags = { no_flags with faults = [ 12 ]; resync = List.range 13 21 } in
  (* an event in flight leaves by 16: fine *)
  check ~flags ~events:[ event ~cycle:15 ~pins:0b01 ~changed:0b01 ~est:9500 () ] [ 10, 0 ];
  (* a replay after the resync: a phantom, and the edge is lost (covered) *)
  check ~flags ~events:[ event ~cycle:50 ~pins:0b01 ~changed:0b01 ~est:9500 () ] [ 10, 0 ];
  [%expect {|
    edges 1: timed 1, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0
    edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 1 (late flag 0) | pending 0
      phantom: cycle 50 pin 0 level true est 9500
    |}]
;;

let%expect_test "an edge flagged at the end of the run is not pending" =
  (* edge at 195, its flag at 196, the run ends at 200 *)
  check ~flags:{ no_flags with pin = [ 196, 0b01 ] } [ 195, 0 ];
  [%expect {| edges 1: timed 0, untimed 0, lost+flagged 1, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 (late flag 0) | pending 0 |}]
;;

let%expect_test "resync conformance (D-13): the first event after a resync carries [lost]" =
  let flags = { no_flags with faults = [ 12 ]; resync = List.range 13 21 } in
  let report events =
    let edges = [ { O.time = cycle_time 10 - 500; pin = 0; level = true } ] in
    let s =
      O.check ~pins:2 ~edges ~events ~flags ~cycle_of_time ~cycle_time ~match_tol:300 ~windows
        ~deadline:(fun _ -> 100) ~end_cycle:200
    in
    printf "%s, ok %b\n" (O.pp_resyncs s) (O.ok s)
  in
  (* the first event after the resync (and after the events in flight) *)
  report [ event ~loss_mark:true ~est:(cycle_time 24) ~cycle:25 ~pins:0b01 ~changed:0 () ];
  report [ event ~est:(cycle_time 24) ~cycle:25 ~pins:0b01 ~changed:0 () ];
  (* an unmarked event still in flight, then a marked one *)
  report
    [ event ~cycle:21 ~pins:0b01 ~changed:0 ()
    ; event ~loss_mark:true ~cycle:25 ~pins:0b01 ~changed:0 ()
    ];
  (* no event after it: pending, not an error; a resync without a pulse
     (start-up) is not counted *)
  report [];
  let flags = { no_flags with resync = List.range 13 21 } in
  let s =
    O.check ~pins:2 ~edges:[] ~events:[ event ~cycle:25 ~pins:0 ~changed:0 () ] ~flags ~cycle_of_time
      ~cycle_time ~match_tol:300 ~windows ~deadline:(fun _ -> 100) ~end_cycle:200
  in
  print_endline (O.pp_resyncs s);
  [%expect {|
    resyncs after a loss 1: first event after it marked lost 1, NOT MARKED 0 (D-13), ok true
    resyncs after a loss 1: first event after it marked lost 0, NOT MARKED 1 (D-13), ok false
    resyncs after a loss 1: first event after it marked lost 1, NOT MARKED 0 (D-13), ok true
    resyncs after a loss 1: first event after it marked lost 0, NOT MARKED 0 (D-13), ok true
    resyncs after a loss 0: first event after it marked lost 0, NOT MARKED 0 (D-13)
    |}]
;;
