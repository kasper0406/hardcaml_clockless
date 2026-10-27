(* Vernier under Event_sim with Liberty delays at typ/slow/fast: the chip
   configuration of Vernier_harness (2 pins x {rise, fall} lines, 40 coarse
   taps of dlygate4sd3/inv/dlygate4sd3/inv, 16 fine taps of buf_1), with the
   Liberty "1k" wire load.

   Expectations never come from the module: per-code delays from the
   closed-form oracle (Vernier_oracle, raw Liberty tables), edge times from
   the ideal target [launch edge + d_align + fraction * T], edge counts and
   order from the requested levels. *)

open! Base
open Stdio
open Hardcaml_clockless
module H = Vernier_harness

let gap = 7 (* cycles between requests: > settle (4) + 2 *)

type sample =
  { pin : int
  ; line : int (* 0 rise line, 1 fall line *)
  ; q_rise : bool (* launch flop Q direction *)
  ; other : bool (* the other line's output level at the XOR *)
  ; delay : int (* fs, launch clock edge to out *)
  }

let modes : (Vernier.Mode.t * Vernier_oracle.mode) list =
  [ Full, Full; Coarse, Coarse_only; Fine, Fine_only ]
;;

let set_mode (t : H.t) mode =
  for pin = 0 to H.pins - 1 do
    H.set_mode t ~pin mode
  done;
  H.wait t gap
;;

(* Launch parity of each line, as the requests toggle them. *)
let parity = Array.init H.pins ~f:(fun _ -> [| false; false |])

(* One full R, F, R, F cycle on every pin with the current select. *)
let four (t : H.t) ?(fraction = fun ~pin:_ ~level:_ -> 0) () =
  List.concat_map [ 1; 0; 1; 0 ] ~f:(fun level ->
    let launches =
      List.init H.pins ~f:(fun pin ->
        let line = if level = 1 then 0 else 1 in
        let q_rise = not parity.(pin).(line) in
        let other = not parity.(pin).(1 - line) in
        parity.(pin).(line) <- q_rise;
        let at = H.request t ~pin ~level ~fraction:(fraction ~pin ~level) () in
        pin, line, q_rise, other, at)
    in
    H.wait t gap;
    List.map launches ~f:(fun (pin, line, q_rise, other, at) ->
      match H.edges_between t ~pin at (at + (gap * H.period)) with
      | [ (time, v) ] ->
        if not (Event_sim.Value.equal v (Event_sim.Value.of_bool (level = 1)))
        then raise_s [%message "wrong level" (pin : int)];
        { pin; line; q_rise; other; delay = time - at }
      | es ->
        raise_s [%message "expected one edge" (pin : int) (es : (int * Event_sim.Value.t) list)]))
;;

let reset_parity () = Array.iter parity ~f:(fun a -> a.(0) <- false; a.(1) <- false)

type sweep =
  { mode : Vernier.Mode.t
  ; coarse : int
  ; fine : int
  ; samples : sample list
  }

(* With identity constants the fraction is the tap: coarse taps in full and
   coarse mode, fine taps in fine mode. *)
let tap_sweep ?(modes = modes) (t : H.t) =
  H.set_constants t H.identity;
  List.concat_map modes ~f:(fun (mode, _) ->
    set_mode t mode;
    let codes =
      match mode with
      | Full | Coarse | Bypass -> List.init H.coarse_taps ~f:(fun c -> c, 0)
      | Fine -> List.init H.fine_taps ~f:(fun f -> 0, f)
    in
    List.map codes ~f:(fun (coarse, fine) ->
      let code = coarse + fine in
      { mode; coarse; fine; samples = four t ~fraction:(fun ~pin:_ ~level:_ -> code) () }))
;;

let ps fs = Event_sim.to_ps fs
let stats xs =
  let n = Float.of_int (List.length xs) in
  let mean = List.sum (module Float) xs ~f:Fn.id /. n in
  List.fold xs ~init:Float.infinity ~f:Float.min, mean, List.fold xs ~init:Float.neg_infinity ~f:Float.max
;;

(* Delays per (mode, pin, line, q_rise) as a function of the swept code. *)
let series sweeps ~mode ~axis ~pin ~line ~q_rise =
  List.filter_map sweeps ~f:(fun s ->
    if not (Vernier.Mode.equal s.mode mode)
    then None
    else (
      let keep =
        match axis with
        | `Coarse -> s.fine = 0
        | `Fine -> s.coarse = 0
      in
      if not keep
      then None
      else
        List.find_map s.samples ~f:(fun x ->
          if x.pin = pin && x.line = line && Bool.equal x.q_rise q_rise
          then Some ((match axis with `Coarse -> s.coarse | `Fine -> s.fine), ps x.delay)
          else None)))
  |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> List.map ~f:snd
  |> Array.of_list
;;

let combos = List.concat_map [ 0; 1 ] ~f:(fun pin -> List.concat_map [ 0; 1 ] ~f:(fun line -> List.map [ true; false ] ~f:(fun q -> pin, line, q)))

let steps a = Array.init (Array.length a - 1) ~f:(fun i -> a.(i + 1) -. a.(i)) |> Array.to_list

let%expect_test "tap codes: delays equal the closed-form Liberty oracle; monotonic; full-cycle coverage" =
  List.iter H.corners ~f:(fun (corner, path) ->
    reset_parity ();
    let t = H.create path in
    let sweeps = tap_sweep t in
    let o = H.oracle path in
    let worst = ref 0. in
    List.iter sweeps ~f:(fun s ->
      let mode = List.Assoc.find_exn modes s.mode ~equal:Vernier.Mode.equal in
      List.iter s.samples ~f:(fun x ->
        let d, rise =
          Vernier_oracle.line_delay o ~mode ~line:x.line ~q_rise:x.q_rise ~other:x.other ~coarse:s.coarse ~fine:s.fine
        in
        ignore (rise : bool);
        let e = (d *. 1e6) -. Float.of_int x.delay in
        worst := Float.max !worst (Float.abs e)));
    let all axis mode = List.map combos ~f:(fun (pin, line, q_rise) -> series sweeps ~mode ~axis ~pin ~line ~q_rise) in
    let monotonic arrs = List.for_all arrs ~f:(fun a -> List.for_all (steps a) ~f:(fun d -> Float.(d > 0.))) in
    let cst = all `Coarse Full |> List.concat_map ~f:steps in
    let fst_ = all `Fine Fine |> List.concat_map ~f:steps in
    let cmin, cmean, cmax = stats cst in
    let fmin, fmean, fmax = stats fst_ in
    let span = all `Coarse Full |> List.map ~f:(fun a -> a.(Array.length a - 1) -. a.(0)) in
    let fspan = all `Fine Fine |> List.map ~f:(fun a -> a.(Array.length a - 1) -. a.(0)) in
    let smin, _, _ = stats span in
    let fsmin, _, _ = stats fspan in
    let latency =
      fst (Vernier_oracle.line_delay o ~mode:Full ~line:0 ~q_rise:true ~other:true ~coarse:(H.coarse_taps - 1) ~fine:(H.fine_taps - 1))
    in
    let zero mode q = (series sweeps ~mode ~axis:`Coarse ~pin:0 ~line:0 ~q_rise:q |> fun a -> if Array.is_empty a then (series sweeps ~mode ~axis:`Fine ~pin:0 ~line:0 ~q_rise:q).(0) else a.(0)) in
    let skew mode = zero mode true -. zero mode false in
    printf
      "%-4s oracle mismatch %.0f fs | monotonic full %b fine %b coarse %b | coarse step %.0f/%.0f/%.0f ps, fine step %.1f/%.1f/%.1f ps (min/mean/max) | coarse span >= %.2f ns (T = 20), fine span >= %.0f ps vs coarse step <= %.0f | latency %.1f ns -> settle_cycles >= %d at 50 MHz\n"
      corner !worst
      (monotonic (all `Coarse Full)) (monotonic (all `Fine Fine))
      (monotonic (all `Coarse Coarse))
      cmin cmean cmax fmin fmean fmax (smin /. 1000.) fsmin cmax latency
      (Float.iround_up_exn (latency *. 1000. /. H.period_ps));
    printf "     zero-code delay %.0f ps (full), launch direction skew rise-fall: full %+.0f coarse %+.0f fine %+.0f ps\n"
      (zero Full true) (skew Full) (skew Coarse) (skew Fine));
  [%expect {|
    typ  oracle mismatch 4 fs | monotonic full true fine true coarse true | coarse step 811/830/890 ps, fine step 64.0/84.0/91.6 ps (min/mean/max) | coarse span >= 32.29 ns (T = 20), fine span >= 1228 ps vs coarse step <= 890 | latency 35.5 ns -> settle_cycles >= 2 at 50 MHz
         zero-code delay 1829 ps (full), launch direction skew rise-fall: full -5 coarse -90 fine +123 ps
    slow oracle mismatch 25 fs | monotonic full true fine true coarse true | coarse step 1297/1327/1407 ps, fine step 106.3/135.9/142.3 ps (min/mean/max) | coarse span >= 51.60 ns (T = 20), fine span >= 1984 ps vs coarse step <= 1407 | latency 56.8 ns -> settle_cycles >= 3 at 50 MHz
         zero-code delay 2902 ps (full), launch direction skew rise-fall: full -14 coarse -143 fine +175 ps
    fast oracle mismatch 15 fs | monotonic full true fine true coarse true | coarse step 549/561/592 ps, fine step 43.2/55.4/58.1 ps (min/mean/max) | coarse span >= 21.85 ns (T = 20), fine span >= 812 ps vs coarse step <= 592 | latency 23.9 ns -> settle_cycles >= 2 at 50 MHz
         zero-code delay 1183 ps (full), launch direction skew rise-fall: full -7 coarse -62 fine +73 ps
    |}]
;;

(* --- Calibration and INL --- *)

(* The sweep the host runs through the loopback with identity constants:
   per line and launch direction, coarse taps in full mode and fine taps in
   fine mode. *)
let lines_of_sweep sweeps =
  Array.init (2 * H.pins) ~f:(fun l ->
    let pin = l / 2
    and line = l % 2 in
    let sweep q_rise : Vernier.Calibration.sweep =
      { coarse = series sweeps ~mode:Full ~axis:`Coarse ~pin ~line ~q_rise
      ; fine = series sweeps ~mode:Fine ~axis:`Fine ~pin ~line ~q_rise
      }
    in
    { Vernier.Calibration.rising = sweep true; falling = sweep false })
;;

let full_sweep (t : H.t) =
  let sweeps = tap_sweep ~modes:[ Full, Full; Fine, Fine_only ] t in
  set_mode t Full;
  lines_of_sweep sweeps
;;

let fit lines = Vernier.Calibration.fit ~coarse_taps:H.coarse_taps ~period_ps:H.period_ps ~mode:Full lines

(* The same sweep predicted by the oracle at a corner (no simulation): the
   "nominal" calibration a host would load before measuring anything. *)
let oracle_lines path =
  let o = H.oracle path in
  Array.init (2 * H.pins) ~f:(fun l ->
    let line = l % 2 in
    let sweep q_rise : Vernier.Calibration.sweep =
      (* the other line's level in the R, F, R, F order of [four] *)
      let other = Bool.equal (line = 0) q_rise in
      let d mode coarse fine =
        fst (Vernier_oracle.line_delay o ~mode ~line ~q_rise ~other ~coarse ~fine) *. 1000.
      in
      { coarse = Array.init H.coarse_taps ~f:(fun c -> d Full c 0)
      ; fine = Array.init H.fine_taps ~f:(fun f -> d Fine_only 0 f)
      }
    in
    { Vernier.Calibration.rising = sweep true; falling = sweep false })
;;

let lsb_ps = H.period_ps /. 256.

(* Every fraction on every line and direction; error against the ideal
   target [reference + a * T / 256]. *)
let mapped_errors (t : H.t) (c : Vernier.Calibration.t) ~fractions =
  H.set_constants t (H.of_calibration c);
  List.concat_map fractions ~f:(fun a ->
    four t ~fraction:(fun ~pin:_ ~level:_ -> a) ()
    |> List.map ~f:(fun x -> a, x, ps x.delay -. (c.d_align_ps +. (Float.of_int a *. lsb_ps))))
;;

let summary errs =
  let es = List.map errs ~f:(fun (_, _, e) -> e) in
  let n = Float.of_int (List.length es) in
  let rms = Float.sqrt (List.sum (module Float) es ~f:(fun e -> e *. e) /. n) in
  let worst = List.fold es ~init:0. ~f:(fun m e -> Float.max m (Float.abs e)) in
  rms, worst
;;

(* Per line and direction, delay against fraction: decreases (non-monotonic
   steps) and the step distribution. *)
let monotonicity errs =
  let by_key = Hashtbl.Poly.create () in
  List.iter errs ~f:(fun (a, x, _) ->
    Hashtbl.add_multi by_key ~key:(x.pin, x.line, x.q_rise) ~data:(a, ps x.delay));
  Hashtbl.fold by_key ~init:([], []) ~f:(fun ~key:_ ~data (decreases, acc) ->
    let xs = List.sort data ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> List.map ~f:snd |> Array.of_list in
    let st = steps xs in
    List.filter st ~f:(fun d -> Float.(d < -0.5)) @ decreases, st @ acc)
;;

let calibrated = Hashtbl.create (module String)

let%expect_test "calibration: INL before (nominal typ constants) and after (fit per corner), with variation" =
  let nominal = fit (oracle_lines (List.Assoc.find_exn H.corners "typ" ~equal:String.equal)) in
  printf
    "nominal (oracle, typ): scale %s offset %s ratio %s d_align %.0f ps\n"
    (Sexp.to_string [%sexp (nominal.scale : int array)])
    (Sexp.to_string [%sexp (nominal.offset : int array)])
    (Sexp.to_string [%sexp (nominal.ratio : int array)])
    nominal.d_align_ps;
  let fractions = List.init 256 ~f:Fn.id in
  List.iter H.corners ~f:(fun (corner, path) ->
    reset_parity ();
    let t = H.create ~config:(H.realistic_config ~seed:11) path in
    let before = mapped_errors t nominal ~fractions:(List.filter fractions ~f:(fun a -> a % 8 = 0)) in
    let c = fit (full_sweep t) in
    Hashtbl.set calibrated ~key:corner ~data:c;
    let after = mapped_errors t c ~fractions in
    let brms, bworst = summary before in
    let arms, aworst = summary after in
    let decreases, st = monotonicity after in
    let smin, smean, smax = stats st in
    let worst_dec = List.fold decreases ~init:0. ~f:Float.min in
    let flags = List.init H.pins ~f:(fun p -> H.flag t "range_fault" p, H.flag t "overrun" p, H.flag t "mismatch" p) in
    printf
      "%-4s fit: scale %s offset %s ratio %s d_align %.0f ps (skew %s ps) | INL before: rms %.0f ps, worst %.0f ps | after: rms %.1f ps (%.2f LSB), worst %.1f ps (%.2f LSB)\n"
      corner
      (Sexp.to_string [%sexp (c.scale : int array)])
      (Sexp.to_string [%sexp (c.offset : int array)])
      (Sexp.to_string [%sexp (c.ratio : int array)])
      c.d_align_ps
      (String.concat ~sep:" " (Array.to_list (Array.map c.skew_ps ~f:(Printf.sprintf "%+.0f"))))
      brms bworst arms (arms /. lsb_ps) aworst (aworst /. lsb_ps);
    printf
      "     per-LSB step %.1f/%.1f/%.1f ps (min/mean/max, LSB %.1f); decreases %d of %d, worst %.1f ps | flags (range, overrun, mismatch) %s | timing violations %d\n"
      smin smean smax lsb_ps (List.length decreases) (List.length st) worst_dec
      (Sexp.to_string [%sexp (flags : (int option * int option * int option) list)])
      (List.length (Event_sim.timing_violations t.sim)));
  [%expect {|
    nominal (oracle, typ): scale (385 385 385 385) offset (0 1 0 1) ratio (4 4 4 4) d_align 1820 ps
    typ  fit: scale (385 385 386 385) offset (3 4 4 0) ratio (3 4 3 4) d_align 2110 ps (skew -8 +22 -10 +26 ps) | INL before: rms 272 ps, worst 405 ps | after: rms 54.3 ps (0.70 LSB), worst 162.5 ps (2.08 LSB)
         per-LSB step -80.4/77.9/194.1 ps (min/mean/max, LSB 78.1); decreases 18 of 2040, worst -80.4 ps | flags (range, overrun, mismatch) (((0)(0)(0))((0)(0)(0))) | timing violations 0
    slow fit: scale (241 241 242 241) offset (3 4 4 0) ratio (3 3 3 3) d_align 3189 ps (skew -12 +35 -16 +42 ps) | INL before: rms 7898 ps, worst 13072 ps | after: rms 71.1 ps (0.91 LSB), worst 218.5 ps (2.80 LSB)
         per-LSB step -38.9/77.8/176.6 ps (min/mean/max, LSB 78.1); decreases 7 of 2040, worst -38.9 ps | flags (range, overrun, mismatch) (((0)(0)(0))((0)(0)(0))) | timing violations 0
    fast fit: scale (570 569 571 569) offset (5 4 5 0) ratio (4 4 4 4) d_align 1474 ps (skew -5 +13 -6 +16 ps) | INL before: rms 3991 ps, worst 6770 ps | after: rms 33.7 ps (0.43 LSB), worst 99.5 ps (1.27 LSB)
         per-LSB step 7.2/78.1/145.6 ps (min/mean/max, LSB 78.1); decreases 0 of 2040, worst 0.0 ps | flags (range, overrun, mismatch) (((0)(0)(0))((0)(0)(0))) | timing violations 0
    |}]
;;

(* --- Glitch-freedom --- *)

type action =
  | Level of int * int (* level, fraction *)
  | Mode of Vernier.Mode.t (* only in idle gaps *)

(* A random script per pin, by cycle: single edges with random fractions
   (same-line edges >= settle + 2 cycles apart), pulses from adjacent-cycle
   rise/fall pairs (1 ns .. 1.5 cycles wide, both polarities: narrower
   requests can come out narrower still, by the two lines' INL, see the next
   test), bursts at a
   fixed fraction toggling every cycle (unchanged selects: no wait), and mode
   switches in idle gaps. *)
let script rng ~cycles =
  let acts = Queue.create () in
  let k = ref 0 in
  let level = ref 0 in
  let mode = ref Vernier.Mode.Full in
  let frac () =
    match !mode with
    | Fine -> Random.State.int rng 16
    | _ -> Random.State.int rng 256
  in
  let edge a =
    level := 1 - !level;
    Queue.enqueue acts (!k, Level (!level, a))
  in
  while !k < cycles do
    (match Random.State.int rng 10 with
     | 0 | 1 | 2 | 3 ->
       edge (frac ());
       k := !k + 7
     | 4 | 5 ->
       (* pulse of width (256 + a2 - a1) / 256 cycles *)
       let w = 13 + Random.State.int rng (if Random.State.bool rng then 370 else 20) in
       let lo = Int.max 0 (256 - w)
       and hi = Int.min 255 (511 - w) in
       let a1 = lo + Random.State.int rng (hi - lo + 1) in
       let a2 = a1 + w - 256 in
       if a2 >= 0 && a2 < 256 && not (Vernier.Mode.equal !mode Fine)
       then (
         edge a1;
         Int.incr k;
         edge a2;
         k := !k + 8)
       else Int.incr k
     | 6 | 7 ->
       let a = frac () in
       for _ = 1 to 6 do
         edge a;
         Int.incr k
       done;
       k := !k + 7
     | _ ->
       let m = List.nth_exn Vernier.Mode.[ Full; Full; Coarse; Fine; Bypass ] (Random.State.int rng 5) in
       mode := m;
       Queue.enqueue acts (!k, Mode m);
       k := !k + 8)
  done;
  Queue.to_list acts
;;

let%expect_test "no glitches or runts: random edges, pulses, bursts and mode switches" =
  List.iter H.corners ~f:(fun (corner, path) ->
    let c = Hashtbl.find_exn calibrated corner in
    let t = H.create ~config:(H.realistic_config ~seed:5) ~constants:(H.of_calibration c) path in
    let o = H.oracle path in
    let bypass_delay = Vernier_oracle.bypass_delay o ~q_rise:true *. 1000. +. 300. in
    let start = H.cycle t + 1 in
    let scripts = List.init H.pins ~f:(fun p -> script (Random.State.make [| p; 42 |]) ~cycles:1500) in
    (* expected output edges per pin: (level, launch, kind) *)
    let expected = Array.init H.pins ~f:(fun _ -> Queue.create ()) in
    let modes = Array.init H.pins ~f:(fun _ -> Vernier.Mode.Full) in
    let by_cycle = Hashtbl.create (module Int) in
    List.iteri scripts ~f:(fun p acts -> List.iter acts ~f:(fun (k, a) -> Hashtbl.add_multi by_cycle ~key:(start + k) ~data:(p, a)));
    let last = 1500 + start + 10 in
    for k = start to last do
      H.goto t k;
      List.iter (List.rev (Hashtbl.find_multi by_cycle k)) ~f:(fun (p, a) ->
        match a with
        | Mode m ->
          modes.(p) <- m;
          H.set_mode t ~pin:p m
        | Level (level, fraction) ->
          let at = H.request t ~pin:p ~level ~fraction () in
          Queue.enqueue expected.(p) (level, at, modes.(p), fraction))
    done;
    let n_edges = ref 0
    and timed_full = ref 0.
    and timed_bypass = ref 0.
    and window = ref 0
    and narrowest = ref Int.max_value in
    let counts_ok =
      List.for_all (List.init H.pins ~f:Fn.id) ~f:(fun p ->
        let got = Queue.to_list t.edges.(p) in
        let exp = Queue.to_list expected.(p) in
        n_edges := !n_edges + List.length exp;
        List.iteri got ~f:(fun i (time, _) ->
          if i > 0 then narrowest := Int.min !narrowest (time - fst (List.nth_exn got (i - 1))));
        List.length got = List.length exp
        && List.for_all2_exn got exp ~f:(fun (time, v) (level, at, m, fraction) ->
          let ok_level = Event_sim.Value.equal v (Event_sim.Value.of_bool (level = 1)) in
          let d = ps (time - at) in
          if Vernier.Mode.equal m Bypass
          then timed_bypass := Float.max !timed_bypass (Float.abs (d -. bypass_delay))
          else if Vernier.Mode.equal m Full
          then timed_full := Float.max !timed_full (Float.abs (d -. c.d_align_ps -. (Float.of_int fraction *. lsb_ps)))
          else if Float.(d < 0. || d > 2. *. H.period_ps) then Int.incr window;
          ok_level))
    in
    let flags = List.init H.pins ~f:(fun p -> H.flag t "overrun" p, H.flag t "range_fault" p, H.flag t "mismatch" p) in
    printf
      "%-4s %d edges: output edges = requested (count, levels) %b | worst error full %.0f ps, bypass %.0f ps; other modes outside window %d | narrowest output pulse %.2f ns | pulse-width violations %d | flags (overrun, range, mismatch) %s\n"
      corner !n_edges counts_ok !timed_full !timed_bypass !window (Event_sim.to_ns !narrowest)
      (List.count (Event_sim.timing_violations t.sim) ~f:(fun v -> String.is_prefix v.instance ~prefix:"sink"))
      (Sexp.to_string [%sexp (flags : (int option * int option * int option) list)]));
  [%expect {|
    typ  687 edges: output edges = requested (count, levels) true | worst error full 203 ps, bypass 18 ps; other modes outside window 0 | narrowest output pulse 0.77 ns | pulse-width violations 0 | flags (overrun, range, mismatch) (((0)(0)(0))((0)(0)(0)))
    slow 687 edges: output edges = requested (count, levels) true | worst error full 306 ps, bypass 26 ps; other modes outside window 0 | narrowest output pulse 0.97 ns | pulse-width violations 0 | flags (overrun, range, mismatch) (((0)(0)(0))((0)(0)(0)))
    fast 687 edges: output edges = requested (count, levels) true | worst error full 231 ps, bypass 10 ps; other modes outside window 0 | narrowest output pulse 0.78 ns | pulse-width violations 0 | flags (overrun, range, mismatch) (((0)(0)(0))((0)(0)(0)))
    |}]
;;

let%expect_test "narrowest pulse: adjacent-cycle rise/fall pairs, full mode" =
  List.iter H.corners ~f:(fun (corner, path) ->
    let c = Hashtbl.find_exn calibrated corner in
    let t = H.create ~config:(H.realistic_config ~seed:5) ~constants:(H.of_calibration c) path in
    let rows =
      List.map [ 1; 2; 3; 4; 6; 8 ] ~f:(fun w ->
        (* rise at fraction 255 - w + 1 ... fall at 1 - w + ...: width w LSB *)
        let a1 = 255 and a2 = w - 1 in
        let at = H.request t ~pin:0 ~level:1 ~fraction:a1 () in
        H.wait t 1;
        ignore (H.request t ~pin:0 ~level:0 ~fraction:a2 () : int);
        H.wait t gap;
        let v0 = List.length (Event_sim.timing_violations t.sim) in
        ignore v0;
        match H.edges_between t ~pin:0 at (at + (3 * H.period)) with
        | [ (r, _); (f, _) ] -> Printf.sprintf "%d LSB: %.0f ps" w (ps (f - r))
        | es -> Printf.sprintf "%d LSB: %d edges" w (List.length es))
    in
    printf "%-4s %s | sink pulse-width violations %d\n" corner (String.concat ~sep:", " rows)
      (List.count (Event_sim.timing_violations t.sim) ~f:(fun v -> String.is_prefix v.instance ~prefix:"sink")));
  [%expect {|
    typ  1 LSB: 0 edges, 2 LSB: 205 ps, 3 LSB: 194 ps, 4 LSB: 291 ps, 6 LSB: 349 ps, 8 LSB: 631 ps | sink pulse-width violations 0
    slow 1 LSB: 0 edges, 2 LSB: 0 edges, 3 LSB: 0 edges, 4 LSB: 217 ps, 6 LSB: 281 ps, 8 LSB: 484 ps | sink pulse-width violations 0
    fast 1 LSB: 94 ps, 2 LSB: 0 edges, 3 LSB: 0 edges, 4 LSB: 193 ps, 6 LSB: 306 ps, 8 LSB: 456 ps | sink pulse-width violations 0
    |}]
;;

(* --- Mode switches --- *)

(* A mode change switches the output mux between the level flop and the
   XOR. Here the bypass muxes and the XOR are slowed until the XOR settles
   after settle_cycles * T, breaking the drain bound (Sdc.vernier): a switch
   from bypass to full at settle_cycles would show the pin the XOR's old
   level for a moment. Mode changes wait two cycles more, as margin; the
   mismatch check flags the slow line. *)
let%expect_test "mode switches wait two cycles beyond the drain bound" =
  let path = List.Assoc.find_exn H.corners "slow" ~equal:String.equal in
  let slow = [ "vernier_p0_r_fbyp", 20.; "vernier_p0_f_fbyp", 20.; "vernier_p0_xor", 20. ] in
  let t = H.create ~config:{ H.base_config with factor_overrides = slow } ~settle:3 ~mode:Bypass path in
  (* identity constants: in full configuration the fraction is the coarse
     tap; 39, the last, is the longest path through the line *)
  let at = H.request t ~pin:0 ~level:1 ~fraction:39 () in
  (* once the request is registered, so the mode cannot apply before it *)
  H.wait t 1;
  H.set_mode t ~pin:0 Full;
  let loopback = ref None in
  Event_sim.on_change t.sim "loopback_0" (fun ~time _ -> if Option.is_none !loopback then loopback := Some time);
  H.wait t 12;
  let edges = H.edges_between t ~pin:0 at (at + (12 * H.period)) in
  printf "XOR settles %.1f ns after the launch edge (settle_cycles * T = 60 ns); mode now %d\n"
    (Event_sim.to_ns (Option.value_exn !loopback - at))
    (Option.value_exn (Event_sim.to_int t.sim "active_mode_0"));
  printf "mismatch %d\n" (Option.value_exn (Event_sim.to_int t.sim "mismatch_0"));
  printf "bypass -> full with the edge in flight: pin edges %s\n"
    (String.concat ~sep:", "
       (List.map edges ~f:(fun (time, v) ->
          Printf.sprintf "%c at %.1f ns" (Event_sim.Value.to_char v) (Event_sim.to_ns (time - at)))));
  [%expect {|
    XOR settles 66.6 ns after the launch edge (settle_cycles * T = 60 ns); mode now 3
    mismatch 1
    bypass -> full with the edge in flight: pin edges 1 at 0.4 ns
    |}]
;;

(* --- Faults --- *)

(* A broken cell: Event_sim's variation factor 1e4 makes it hold its output
   for microseconds, and with inertial delays it never follows an input that
   toggles faster than that: stuck at its level. *)
let fault_run ~corner ~path ~instance =
  let c = Hashtbl.find_exn calibrated corner in
  let config = { (H.realistic_config ~seed:5) with factor_overrides = [ instance, 1e4 ] } in
  let t = H.create ~config ~constants:(H.of_calibration c) path in
  (* let the broken cell settle from its power-up X *)
  H.wait t 2000;
  let o = H.oracle path in
  let bypass_delay = Vernier_oracle.bypass_delay o ~q_rise:true *. 1000. +. 300. in
  List.map
    [ "full", Vernier.Mode.Full; "coarse", Coarse; "fine", Fine; "bypass", Bypass ]
    ~f:(fun (name, mode) ->
      H.set_mode t ~pin:0 mode;
      H.wait t gap;
      H.clear_flags t ~pin:0 1;
      H.mode_write t ~pin:0 1;
      H.wait t 1;
      H.clear_flags t ~pin:0 0;
      H.mode_write t ~pin:0 0;
      let bypass = Vernier.Mode.equal mode Bypass in
      let fraction k = if Vernier.Mode.equal mode Fine then k % 16 else (k * 37) % 256 in
      let ok =
        List.init 16 ~f:(fun k ->
          let level = 1 - t.level.(0) in
          let fraction = fraction k in
          let at = H.request t ~pin:0 ~level ~fraction () in
          H.wait t gap;
          match H.edges_between t ~pin:0 at (at + (gap * H.period)) with
          | [ (time, v) ] ->
            let d = ps (time - at) in
            Event_sim.Value.equal v (Event_sim.Value.of_bool (level = 1))
            && (if bypass
                then Float.(abs (d -. bypass_delay) < 100.)
                else if Vernier.Mode.equal mode Full
                then Float.(abs (d -. c.d_align_ps -. (Float.of_int fraction *. lsb_ps)) < 400.)
                else Float.(d > 0. && d < 2. *. H.period_ps))
          | _ -> false)
        |> List.count ~f:Fn.id
      in
      (* re-align the requested level with the pad when edges went missing *)
      Printf.sprintf "%s %2d/16%s" name ok (if Option.equal Int.equal (H.flag t "mismatch" 0) (Some 1) then " (mismatch)" else ""))
;;

let%expect_test "faults: each fidelity mode works when the parts it bypasses are broken" =
  List.iter H.corners ~f:(fun (corner, path) ->
    List.iter
      [ "coarse stage 5", "vernier_p0_r_c_stage5_0"
      ; "fine stage 3", "vernier_p0_r_f_stage3_0"
      ; "coarse tree", "vernier_p0_f_cm6_0"
      ; "xor", "vernier_p0_xor"
      ]
      ~f:(fun (what, instance) ->
        printf "%-4s %-14s %s\n" corner what (String.concat ~sep:", " (fault_run ~corner ~path ~instance))));
  [%expect {|
    typ  coarse stage 5 full  9/16 (mismatch), coarse  9/16 (mismatch), fine 16/16, bypass 16/16 (mismatch)
    typ  fine stage 3   full 12/16 (mismatch), coarse 16/16, fine  7/16 (mismatch), bypass 16/16 (mismatch)
    typ  coarse tree    full  4/16 (mismatch), coarse  4/16 (mismatch), fine 16/16, bypass 16/16 (mismatch)
    typ  xor            full  0/16 (mismatch), coarse  0/16 (mismatch), fine  0/16 (mismatch), bypass 16/16 (mismatch)
    slow coarse stage 5 full 10/16 (mismatch), coarse 10/16 (mismatch), fine 16/16, bypass 16/16 (mismatch)
    slow fine stage 3   full  8/16 (mismatch), coarse 16/16, fine  7/16 (mismatch), bypass 16/16 (mismatch)
    slow coarse tree    full  4/16 (mismatch), coarse  4/16 (mismatch), fine 16/16, bypass 16/16 (mismatch)
    slow xor            full  0/16 (mismatch), coarse  0/16 (mismatch), fine  0/16 (mismatch), bypass 16/16 (mismatch)
    fast coarse stage 5 full  7/16 (mismatch), coarse  7/16 (mismatch), fine 16/16, bypass 16/16 (mismatch)
    fast fine stage 3   full  9/16 (mismatch), coarse 16/16, fine  7/16 (mismatch), bypass 16/16 (mismatch)
    fast coarse tree    full  4/16 (mismatch), coarse  4/16 (mismatch), fine 16/16, bypass 16/16 (mismatch)
    fast xor            full  0/16 (mismatch), coarse  0/16 (mismatch), fine  0/16 (mismatch), bypass 16/16 (mismatch)
    |}]
;;

let%expect_test "the settle rule: dense select changes glitch without it; with it they are overruns and the pin falls back to bypass" =
  let corner, path = List.hd_exn H.corners in
  let c = Hashtbl.find_exn calibrated corner in
  List.iter [ 0; 4 ] ~f:(fun settle ->
    let t = H.create ~config:(H.realistic_config ~seed:5) ~settle ~constants:(H.of_calibration c) path in
    let rng = Random.State.make [| 3 |] in
    let n = 200 in
    let first = (H.cycle t + 3) * H.period in
    for _ = 1 to n do
      (* an edge every cycle: each line every 2, with a new select *)
      ignore (H.request t ~pin:0 ~level:(1 - t.level.(0)) ~fraction:(Random.State.int rng 256) () : int);
      H.wait t 1
    done;
    H.wait t 10;
    let got = H.edges_between t ~pin:0 first Int.max_value in
    let widths = List.zip_exn (List.drop_last_exn got) (List.tl_exn got) |> List.map ~f:(fun ((a, _), (b, _)) -> b - a) in
    let runts = List.count widths ~f:(fun w -> w < Event_sim.of_ps 300.) in
    printf
      "settle_cycles %d: %d requested edges -> %d output edges, %d pulses < 0.3 ns, sink pulse-width violations %d, overrun %s, fell back to bypass %s\n"
      settle n (List.length got) runts
      (List.count (Event_sim.timing_violations t.sim) ~f:(fun v -> String.is_prefix v.instance ~prefix:"sink"))
      (Sexp.to_string [%sexp (H.flag t "overrun" 0 : int option)])
      (Sexp.to_string [%sexp (H.flag t "fallback" 0 : int option)]));
  [%expect {|
    settle_cycles 0: 200 requested edges -> 204 output edges, 1 pulses < 0.3 ns, sink pulse-width violations 0, overrun (0), fell back to bypass (0)
    settle_cycles 4: 200 requested edges -> 200 output edges, 0 pulses < 0.3 ns, sink pulse-width violations 0, overrun (1), fell back to bypass (1)
    |}]
;;
