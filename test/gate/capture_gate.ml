(* Pin_capture gate run: random edges on 4 pins at typ/slow/fast with the
   real circuit, in chunks (each chunk a fresh simulation with its own seed,
   so the run can be split). Reports per corner: oracle summary (unflagged
   errors), calibrated single-edge error, snapshot race margin, status.

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
  List.iter Hardcaml_clockless_test.Capture_timing.corners ~f:(fun (corner, file) ->
    let path = "test/" ^ file in
    let t0 = Unix.gettimeofday () in
    let totals = Array.create ~len:8 0 in
    let rms_acc = ref 0. and n_acc = ref 0 and worst = ref 0. and bin = ref 0. and outliers = ref 0 in
    let race_min = ref Float.infinity in
    let lat_all = ref [] and lat_iso = ref [] in
    let errors = ref [] in
    let status = Hashtbl.create (module String) in
    let chunks = (per_corner + chunk - 1) / chunk in
    for k = 0 to chunks - 1 do
      let edges =
        T.random_edges ~min_same_pin_ns:min_same ~seed:((seed * 1000) + k) ~count:chunk ~start_ns:800.
          ~mean_gap_ns:60. ~near ()
      in
      let cycles = (List.last_exn edges |> fst) / T.clock_period + 450 in
      let race = ref None in
      let r = T.simulate ~seed:k ~wire_load ~path ~edges ~cycles ~setup:(fun sim -> race := Some (T.watch_race sim)) () in
      let t = T.analyse r ~edges in
      let all, isolated = T.latencies t ~edges in
      lat_all := all @ !lat_all;
      lat_iso := isolated @ !lat_iso;
      let s = t.summary in
      List.iteri
        [ s.edges; s.timed; s.untimed; s.flagged_lost; s.flagged_bad; s.unflagged_lost; s.unflagged_phantom; s.pending ]
        ~f:(fun i v -> totals.(i) <- totals.(i) + v);
      errors := !errors @ s.errors;
      rms_acc := !rms_acc +. (t.rms_class_ps *. t.rms_class_ps *. Float.of_int t.samples);
      n_acc := !n_acc + t.samples;
      worst := Float.max !worst t.worst_class_ps;
      outliers := !outliers + t.outliers;
      bin := t.period /. Float.of_int T.bins;
      let m = T.race_margins (Option.value_exn !race) ~edges |> List.map ~f:Event_sim.to_ps in
      race_min := List.fold m ~init:!race_min ~f:Float.min;
      List.iter r.status ~f:(fun (nm, v) ->
        if String.is_suffix nm ~suffix:"count" then Hashtbl.update status nm ~f:(fun o -> Option.value o ~default:0 + v));
      eprintf "%s chunk %d/%d done (%.0fs)\n%!" corner (k + 1) chunks (Unix.gettimeofday () -. t0)
    done;
    let rms = Float.sqrt (!rms_acc /. Float.of_int !n_acc) in
    printf
      "%-4s edges %d: timed %d, untimed %d, lost+flagged %d, bad+flagged %d | UNFLAGGED: lost %d, phantom %d | pending %d\n"
      corner totals.(0) totals.(1) totals.(2) totals.(3) totals.(4) totals.(5) totals.(6) totals.(7);
    printf
      "     single-edge error (per-class offsets): %d events, rms %.1f ps (%.2f bins), worst %.1f ps (%.2f bins), >3 bins %d; race margin min %.0f ps\n"
      !n_acc rms (rms /. !bin) !worst (!worst /. !bin) !outliers !race_min;
    printf "     edge -> event latency, clock edges (first after the edge = 1): all %s\n"
      (T.pp_latencies !lat_all);
    printf "       no other edge within 100 ns: %s\n" (T.pp_latencies !lat_iso);
    printf "     counts: %s\n"
      (Hashtbl.to_alist status |> List.sort ~compare:Poly.compare
       |> List.map ~f:(fun (k, v) -> Printf.sprintf "%s=%d" k v) |> String.concat ~sep:" ");
    List.iter (List.take !errors 10) ~f:(printf "     %s\n");
    printf "     (%.0f s)\n%!" (Unix.gettimeofday () -. t0))
;;
