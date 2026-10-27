(* Pin_capture gate run: random edges on 4 pins at typ/slow/fast with the
   real circuit, in chunks (each chunk a fresh simulation with its own seed,
   so the run can be split), with a quiet stretch every 50 edges so that the
   oracle's flag deadlines are bounded (Capture_oracle). Per corner, the
   calibration (bins, delay, per-class offsets) is fitted to a separate run
   first and then frozen. Reports per corner: oracle accounting (unflagged
   errors, late flags), timing (edges validated against their true time and
   the error distribution of those), per-pin flag latency, snapshot race
   margin, the D-13 loss marks after resyncs, the torn-record and
   plausibility flags against the true edges (conservative and false flags),
   the plausibility ages of unflagged events, status counts. Exits 1 on any
   unflagged error, unjudged edge, resync without a [lost] mark on the
   event after it, or unknown (X) flag.

   Usage: capture_gate.exe [EDGES_PER_CORNER [SEED [NEAR [MIN_SAME_PIN_NS [SLOTS [WIRES]]]]]]
   SLOTS is the slots per bank (2..4, default 2); WIRES is [1k] (the
   library's default wire load, the baseline) or [flat] (1 fF per net). *)

open! Base
open Stdio
open Hardcaml_clockless

let () =
  let argv = Sys.get_argv () in
  let slots = if Array.length argv > 5 then Int.of_string argv.(5) else 2 in
  let module T =
    Hardcaml_clockless_test.Capture_timing.Make (struct
      let slots = slots
    end)
  in
  let wire_load : Event_sim.Config.wire_load =
    match if Array.length argv > 6 then argv.(6) else "1k" with
    | "1k" -> Liberty_default
    | "flat" -> Flat
    | w -> raise_s [%message "WIRES: 1k or flat" w]
  in
  printf "slots per bank: %d, wires: %s\n" slots
    (match wire_load with
     | Flat -> "flat 1 fF"
     | _ -> "Liberty default (1k)");
  let per_corner = if Array.length argv > 1 then Int.of_string argv.(1) else 30000 in
  let seed = if Array.length argv > 2 then Int.of_string argv.(2) else 1 in
  let near = if Array.length argv > 3 then Float.of_string argv.(3) else 0.1 in
  let min_same = if Array.length argv > 4 then Float.of_string argv.(4) else 5. in
  let chunk = 5000 in
  let failed = ref false in
  let run ~path ~seed ~sim_seed ~count ~setup =
    let edges =
      T.random_edges ~min_same_pin_ns:min_same ~quiet_every:50 ~seed ~count ~start_ns:800.
        ~mean_gap_ns:60. ~near ()
    in
    let cycles = (List.last_exn edges |> fst) / T.clock_period + 450 in
    edges, T.simulate ~seed:sim_seed ~wire_load ~path ~edges ~cycles ~setup ()
  in
  List.iter Hardcaml_clockless_test.Capture_timing.corners ~f:(fun (corner, file) ->
    let path = "test/" ^ file in
    let t0 = Unix.gettimeofday () in
    (* Calibration run: its own seed, not validated. *)
    let calibration =
      let edges, r = run ~path ~seed:((seed * 1000) + 999) ~sim_seed:999 ~count:chunk ~setup:(fun _ -> ()) in
      T.fit r ~edges
    in
    let totals = Array.create ~len:13 0 in
    let singles = ref [] and multi = ref [] and latencies = ref [] in
    let bin = ref 0. in
    let race_min = ref Float.infinity in
    let lat_all = ref [] and lat_iso = ref [] in
    let x_outputs = ref 0 in
    let audit = Array.create ~len:5 0 in
    let ages = ref [] in
    let pulse_width = ref 0 in
    let x_resolved = ref 0 in
    let errors = ref [] in
    let status = Hashtbl.create (module String) in
    let chunks = (per_corner + chunk - 1) / chunk in
    for k = 0 to chunks - 1 do
      let race = ref None in
      let edges, r =
        run ~path ~seed:((seed * 1000) + k) ~sim_seed:k ~count:chunk
          ~setup:(fun sim -> race := Some (T.watch_race sim))
      in
      let t = T.analyse ~calibration r ~edges in
      let a = T.flag_audit r ~edges ~est:(T.estimate_with r calibration ~k_offset:t.k_offset) in
      List.iteri [ a.torn; a.torn_right; a.implausible; a.implausible_alone; a.implausible_alone_right ]
        ~f:(fun i v -> audit.(i) <- audit.(i) + v);
      ages := a.ages @ !ages;
      x_resolved := !x_resolved + Event_sim.reg_x_resolutions r.sim;
      (* minimum pulse width violations on the capture's clock pins *)
      pulse_width
      := !pulse_width
         + List.count (Event_sim.timing_violations r.sim) ~f:(fun v ->
           String.is_prefix v.instance ~prefix:"cap_"
           &&
           match v.kind with
           | Pulse_width _ -> true
           | Recovery_removal _ -> false);
      let all, isolated = T.latencies t ~edges in
      lat_all := all @ !lat_all;
      lat_iso := isolated @ !lat_iso;
      let s = t.summary in
      x_outputs := !x_outputs + r.x_outputs;
      List.iteri
        [ s.edges; s.timed; s.untimed; s.flagged_lost; s.flagged_bad; s.unflagged_lost
        ; s.unflagged_phantom; s.late; s.pending; s.no_estimate; s.resyncs; s.resync_marked
        ; s.resync_unmarked ]
        ~f:(fun i v -> totals.(i) <- totals.(i) + v);
      errors := !errors @ s.errors;
      singles := t.singles @ !singles;
      multi := t.multi @ !multi;
      latencies := s.latencies @ !latencies;
      bin := t.period /. Float.of_int T.bins;
      let m = T.race_margins ~ptr_resets:r.ptr_resets (Option.value_exn !race) ~edges |> List.map ~f:Event_sim.to_ps in
      race_min := List.fold m ~init:!race_min ~f:Float.min;
      List.iter r.status ~f:(fun (nm, v) ->
        if String.is_suffix nm ~suffix:"count" then Hashtbl.update status nm ~f:(fun o -> Option.value o ~default:0 + v));
      eprintf "%s chunk %d/%d done (%.0fs)\n%!" corner (k + 1) chunks (Unix.gettimeofday () -. t0)
    done;
    printf
      "%-4s edges %d: timed %d, untimed %d, lost+flagged %d, bad+flagged %d | UNFLAGGED: lost %d, phantom %d (late flag %d) | pending %d\n"
      corner totals.(0) totals.(1) totals.(2) totals.(3) totals.(4) totals.(5) totals.(6) totals.(7)
      totals.(8);
    printf "     %s\n"
      (T.pp_timing_stats ~bin:!bin ~edges:totals.(0) ~no_estimate:totals.(9) !singles !multi);
    printf "     per-pin flag latency: %d errors covered, max %d cycles (deadline: end of the first %d-cycle quiet stretch); race margin min %.0f ps\n"
      (List.length !latencies) (List.fold !latencies ~init:0 ~f:Int.max) T.quiet_cycles !race_min;
    printf "     edge -> event latency, clock edges (first at or after the edge = 1): all %s\n"
      (T.pp_latencies !lat_all);
    printf "       no other edge within 100 ns: %s\n" (T.pp_latencies !lat_iso);
    printf "     resyncs after a loss %d: first event after it marked lost %d, NOT MARKED %d (D-13)\n"
      totals.(10) totals.(11) totals.(12);
    printf "     counts: %s\n"
      (Hashtbl.to_alist status |> List.sort ~compare:Poly.compare
       |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v) |> String.concat ~sep:" ");
    List.iter (List.take !errors 10) ~f:(printf "     %s\n");
    printf "     (%.0f s)\n%!" (Unix.gettimeofday () -. t0);
    printf "     X on flags or event payloads: %d\n" !x_outputs;
    printf
      "     torn records %d (%d of them decoded to a right time: conservative); implausible %d, %d with no other flag (%d of those decoded right: false flags)\n"
      audit.(0) audit.(1) audit.(2) audit.(3) audit.(4);
    printf "     plausibility age (capture to check), unflagged timed events: %s cycles (limit %d)\n"
      (let a = List.sort !ages ~compare:Float.compare |> Array.of_list in
       let n = Array.length a in
       if n = 0 then "none"
       else Printf.sprintf "min %.2f, median %.2f, p99 %.2f, max %.2f" a.(0) a.(n / 2) a.(n * 99 / 100) a.(n - 1))
      T.Cap.plausible_cycles;
    printf "     minimum pulse width violations on capture flops (logged): %d; register bits that loaded X (resolved at random): %d\n" !pulse_width !x_resolved;
    if totals.(5) + totals.(6) + totals.(8) + totals.(12) + !x_outputs > 0 then failed := true);
  if !failed then Stdlib.exit 1
;;
