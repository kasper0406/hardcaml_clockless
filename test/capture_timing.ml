(* The real circuit for Event_sim: a fixed 17-stage ring with flop-cell
   counters and clk sampler, plus Pin_capture built from hand-placed cells,
   drained by a 50 MHz clock. Shared by the timing tests and the bench. *)

open! Base
open Hardcaml
open Hardcaml_clockless

let corners =
  [ "typ", "data/sg13cmos5l_excerpt.lib"
  ; "slow", "data/sg13cmos5l_excerpt_slow_1p08V_125C.lib"
  ; "fast", "data/sg13cmos5l_excerpt_fast_1p32V_m40C.lib"
  ]
;;

module R = Ring.Make (struct
    let lengths = [ 17 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 9
  end)

module Cfg = struct
  let pins = 4
  let slots = 2
  let guard_cycles = 4
  let order_tolerance = 4
  let status_width = 16
  let model_captures = 4
  let clock_leaf_fanout = 12
end

module Cap = Pin_capture.Make (Cfg) (R)

let n = 17
let clock_period = Event_sim.of_ns 20.
let name = "cap"

let dfrbpq : R.flop =
  { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
;;

let circuit (library : Cell_library.t) =
  let enable = Signal.input "enable" 1 in
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  let pins = Signal.input "pins" Cfg.pins in
  let pin_enable = Signal.input "pin_enable" Cfg.pins in
  let length_sel = Signal.zero R.select_width in
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
  let sampled =
    R.Clk_sampler.create
      ~first_stage:(library, dfrbpq)
      ~name:"ring"
      scope
      { clock; clear = Signal.gnd; enable; length_sel; state }
  in
  let o =
    Cap.create
      ~first_stage:(library, dfrbpq)
      ~name
      (Cells
         { library
         ; nand2 = "sg13cmos5l_nand2_1"
         ; xor2 = "sg13cmos5l_xor2_1"
         ; inverter = "sg13cmos5l_inv_1"
         ; buffer = "sg13cmos5l_buf_1"
         ; mux2 = "sg13cmos5l_mux2_1"
         ; flop = dfrbpq
         ; delay = "sg13cmos5l_dlygate4sd3_1", 2
         })
      scope
      { clock
      ; clear
      ; clear_status = Signal.gnd
      ; pins
      ; pin_enable
      ; length_sel
      ; raw_select = Signal.zero Cap.raw_select_width
      ; ring = state
      }
  in
  Circuit.create_exn
    ~name:"capture_timing"
    (Signal.output "node0" (Signal.bit state.nodes ~pos:0)
     :: Signal.output "smp_valid" sampled.valid
     :: Signal.output "smp_phase" sampled.phase
     :: Cap.O.(to_list (map2 port_names o ~f:Signal.output)))
;;

let config ?(seed = 0) () = { Event_sim.Config.default with seed }

(* One run: [edges] are (time fs, pin) toggles. Samples the outputs every
   cycle (mid-cycle, after the edge they were registered on). *)
type event =
  { cycle : int
  ; untimed : bool
  ; pins : int
  ; changed : int
  ; reconfig : int
  ; stale : int
  ; flagged : bool
  ; bank : int
  ; time : int
  ; phase : int
  ; periods : int
  }

type run =
  { events : event list
  ; pin_flags : (int * int) list
  ; global_flags : int list
  ; status : (string * int) list
  ; rises : int array (* node0 rising edges *)
  ; sim : Event_sim.t
  ; cycles : int
  ; clk_phases : int list (* clk sampler phases, for calibration *)
  ; x_outputs : int (* X on a flag or event_valid after start-up *)
  }

let status_names =
  [ "capture_fault"; "fault_count"; "overflow"; "overflow_count"; "lost_edge"; "lost_edge_count"
  ; "level_mismatch"; "level_mismatch_count"; "merge_error"; "merge_error_count"; "empty_count"
  ; "stale_count"; "runt_count"
  ; "resyncing"
  ]
;;

let get sim nm = Option.value (Event_sim.to_int sim nm) ~default:(-1)

let simulate ?(seed = 0) ?(setup = fun (_ : Event_sim.t) -> ()) ~path ~(edges : (int * int) list) ~cycles () =
  let tl = Timing_library.load path in
  let sim = Event_sim.create ~config:(config ~seed ()) tl (circuit tl.library) in
  Event_sim.set_input_int sim "enable" 0 ~at:0;
  Event_sim.set_input_int sim "enable" 1 ~at:(Event_sim.of_ns 5.);
  Event_sim.set_input_int sim "clear" 1 ~at:0;
  Event_sim.set_input_int sim "clear" 0 ~at:(3 * clock_period);
  Event_sim.set_input_int sim "pins" 0 ~at:0;
  Event_sim.set_input_int sim "pin_enable" ((1 lsl Cfg.pins) - 1) ~at:0;
  Event_sim.add_clock sim "clock" ~period:clock_period;
  let level = Array.create ~len:Cfg.pins 0 in
  List.iter (List.sort edges ~compare:(fun (a, _) (b, _) -> Int.compare a b)) ~f:(fun (t, p) ->
    level.(p) <- 1 - level.(p);
    Event_sim.set_input sim (Printf.sprintf "pins[%d]" p) (if level.(p) = 1 then V1 else V0) ~at:t);
  let rises = Queue.create () in
  Event_sim.on_change sim "node0" (fun ~time v ->
    match v with
    | V1 -> Queue.enqueue rises time
    | _ -> ());
  setup sim;
  let events = Queue.create () in
  let pin_flags = Queue.create () in
  let global_flags = Queue.create () in
  let clk_phases = Queue.create () in
  let x_outputs = ref 0 in
  for c = 1 to cycles do
    Event_sim.run_until sim ((c * clock_period) + (clock_period / 2));
    if get sim "smp_valid" = 1 && c > 10 then Queue.enqueue clk_phases (get sim "smp_phase");
    if get sim "event_valid" = 1
    then (
      let g nm = get sim ("event_" ^ nm) in
      Queue.enqueue
        events
        { cycle = c
        ; untimed = g "untimed" = 1
        ; pins = g "pins"
        ; changed = g "changed"
        ; reconfig = g "reconfig"
        ; stale = g "stale"
        ; flagged =
            g "fault" <> 0 || g "overflow" <> 0 || g "order_error" <> 0 || g "pattern_error" <> 0
        ; bank = g "bank"
        ; time = g "time"
        ; phase = g "phase"
        ; periods = g "periods"
        });
    if c > 8
    then (
      let flag nm =
        let v = get sim nm in
        if v < 0 then Int.incr x_outputs;
        Int.max v 0
      in
      let pf =
        flag "status_lost_edge_now"
        lor flag "status_level_mismatch_now"
        lor flag "status_merge_error_now"
      in
      if pf <> 0 then Queue.enqueue pin_flags (c, pf);
      if flag "status_fault_now" <> 0
         || flag "status_overflow_now" <> 0
         || flag "status_runt_now" <> 0
         || flag "status_resyncing" <> 0
      then Queue.enqueue global_flags c;
      if get sim "event_valid" < 0 then Int.incr x_outputs)
  done;
  { events = Queue.to_list events
  ; pin_flags = Queue.to_list pin_flags
  ; global_flags = Queue.to_list global_flags
  ; status = List.map status_names ~f:(fun nm -> nm, get sim ("status_" ^ nm))
  ; rises = Queue.to_array rises
  ; sim
  ; cycles
  ; clk_phases = Queue.to_list clk_phases
  ; x_outputs = !x_outputs
  }
;;

(* ---- Analysis ------------------------------------------------------------ *)

let range_periods = 1 lsl R.count_width
let bins = 2 * n

(* Mean ring period (fs) from node0's rising edges. *)
let period_fs (r : run) =
  let xs = Array.sub r.rises ~pos:10 ~len:(Array.length r.rises - 10) in
  Float.of_int (xs.(Array.length xs - 1) - xs.(0)) /. Float.of_int (Array.length xs - 1)
;;

(* Code density: bin widths (fs) from a phase histogram of samples at times
   uncorrelated with the ring. *)
let calibrate ~period phases =
  let hist = Array.create ~len:bins 0 in
  List.iter phases ~f:(fun p -> if p >= 0 && p < bins then hist.(p) <- hist.(p) + 1);
  let total = Float.of_int (List.length phases) in
  let width = Array.map hist ~f:(fun h -> Float.of_int h /. total *. period) in
  let start = Array.create ~len:bins 0. in
  for p = 1 to bins - 1 do
    start.(p) <- start.(p - 1) +. width.(p - 1)
  done;
  width, start
;;

(* Absolute time estimate of a timed event: the node0 rise that starts its
   period plus the calibrated position of its phase bin. [k_offset] relates
   the counter value to the index of node0 rises (the counters start at 0
   or 1 depending on the enable race). Unwrapped against the output time. *)
let estimate (r : run) ~width ~start ~k_offset (e : event) =
  let t_out = e.cycle * clock_period in
  (* Index of the last rise before the output. *)
  let last =
    match
      Array.binary_search r.rises ~compare:Int.compare `Last_less_than_or_equal_to t_out
    with
    | Some i -> i
    | None -> 0
  in
  let k_now = last + k_offset in
  let back = (k_now - e.periods) % range_periods in
  let idx = last - back in
  if idx < 0 || idx >= Array.length r.rises || e.phase < 0 || e.phase >= bins
  then None
  else Some (r.rises.(idx) + Float.iround_nearest_exn (start.(e.phase) +. (width.(e.phase) /. 2.)))
;;

let with_levels (edges : (int * int) list) =
  let lv = Array.create ~len:Cfg.pins false in
  List.sort edges ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> List.map ~f:(fun (time, pin) ->
    lv.(pin) <- not lv.(pin);
    { Capture_oracle.time; pin; level = lv.(pin) })
;;

let oracle (r : run) ~edges ~est ~match_tol =
  Capture_oracle.check
    ~pins:Cfg.pins
    ~edges:(with_levels edges)
    ~events:
      (List.map r.events ~f:(fun e ->
         { Capture_oracle.cycle = e.cycle
         ; untimed = e.untimed
         ; pins = e.pins
         ; changed = e.changed
         ; reconfig = e.reconfig
         ; stale = e.stale
         ; flagged = e.flagged
         ; est = (if e.untimed then None else est e)
         }))
    ~pin_flags:r.pin_flags
    ~global_flags:r.global_flags
    ~cycle_of_time:(fun t -> (t + clock_period - 1) / clock_period)
    ~cycle_time:(fun c -> c * clock_period)
    ~match_tol
    ~back:4
    ~fwd:400
    ~end_cycle:r.cycles
;;

type timing =
  { summary : Capture_oracle.summary
  ; period : float
  ; bin_min : float
  ; bin_max : float
  ; offset_ps : float
  ; rms_ps : float
  ; worst_ps : float
  ; rms_class_ps : float (* after per (pin, polarity, bank) offsets from the other half *)
  ; worst_class_ps : float
  ; samples : int
  ; outliers : int (* |error| > 3 mean bins *)
  ; outlier_list : (int * int * float) list (* output cycle, pin, error ps *)
  ; k_offset : int
  }

let analyse ?(calib = `Captures) (r : run) ~edges =
  let period = period_fs r in
  let phases =
    match calib with
    | `Captures -> List.filter_map r.events ~f:(fun e -> if e.untimed then None else Some e.phase)
    | `Clk_sampler -> r.clk_phases
  in
  let width, start = calibrate ~period phases in
  (* Pass 1: wide tolerance, find the counter offset and the constant delay. *)
  let try_offset k_offset =
    let est e = estimate r ~width ~start ~k_offset e in
    let s = oracle r ~edges ~est ~match_tol:(Event_sim.of_ns 3.) in
    let res =
      List.filter_map s.matches ~f:(fun (ev, edge, k) ->
        if k = 1 then Option.map ev.est ~f:(fun t -> edge.time - t) else None)
    in
    s.timed, res, k_offset
  in
  let _, res, k_offset =
    List.map [ -2; -1; 0; 1; 2 ] ~f:try_offset
    |> List.max_elt ~compare:(fun (a, _, _) (b, _, _) -> Int.compare a b)
    |> Option.value_exn
  in
  let median xs =
    let a = List.sort xs ~compare:Int.compare |> Array.of_list in
    if Array.is_empty a then 0 else a.(Array.length a / 2)
  in
  let offset = median res in
  (* Pass 2: with the offset; attribution tolerance 2 ns. *)
  let est e = Option.map (estimate r ~width ~start ~k_offset e) ~f:(fun t -> t + offset) in
  let summary = oracle r ~edges ~est ~match_tol:(Event_sim.of_ns 2.) in
  let singles =
    List.filter_map summary.matches ~f:(fun (ev, edge, k) ->
      if k = 1
      then (
        let e = List.find_exn r.events ~f:(fun x -> x.cycle = ev.cycle) in
        Option.map ev.est ~f:(fun t -> (edge.pin, edge.level, e.bank, ev.cycle), Float.of_int (edge.time - t)))
      else None)
  in
  let stats xs =
    let nn = Float.of_int (List.length xs) in
    let rms = Float.sqrt (List.sum (module Float) xs ~f:(fun x -> x *. x) /. nn) in
    let worst = List.fold xs ~init:0. ~f:(fun a x -> Float.max a (Float.abs x)) in
    rms, worst
  in
  let errs = List.map singles ~f:snd in
  let mean_err = List.sum (module Float) errs ~f:Fn.id /. Float.of_int (Int.max 1 (List.length errs)) in
  let errs = List.map errs ~f:(fun x -> x -. mean_err) in
  let rms, worst = stats errs in
  (* Per-class offsets estimated on even samples, applied to odd ones. *)
  let first, second = List.partition_tf (List.mapi singles ~f:(fun i x -> i, x)) ~f:(fun (i, _) -> i % 2 = 0) in
  let tbl = Hashtbl.Poly.create () in
  let cls (p, l, b, _) = p, l, b in
  List.iter first ~f:(fun (_, (c, e)) -> Hashtbl.add_multi tbl ~key:(cls c) ~data:e);
  let class_mean c =
    match Hashtbl.find tbl c with
    | Some xs -> List.sum (module Float) xs ~f:Fn.id /. Float.of_int (List.length xs)
    | None -> mean_err
  in
  let rms_class, worst_class = stats (List.map second ~f:(fun (_, (c, e)) -> e -. class_mean (cls c))) in
  let bin = period /. Float.of_int bins in
  let ps x = x /. 1000. in
  { summary
  ; period = ps period
  ; bin_min = ps (Array.fold width ~init:Float.infinity ~f:Float.min)
  ; bin_max = ps (Array.fold width ~init:0. ~f:Float.max)
  ; offset_ps = ps (Float.of_int offset +. mean_err)
  ; rms_ps = ps rms
  ; worst_ps = ps worst
  ; rms_class_ps = ps rms_class
  ; worst_class_ps = ps worst_class
  ; samples = List.length errs
  ; outliers = List.count errs ~f:(fun x -> Float.(abs x > 3. *. bin))
  ; outlier_list =
      List.filter_map singles ~f:(fun ((p, _, _, c), e) ->
        let e = e -. mean_err in
        if Float.(abs e > 3. *. bin) then Some (c, p, ps e) else None)
  ; k_offset
  }
;;

let pp_timing ~corner t =
  let bin = t.period /. Float.of_int bins in
  Printf.sprintf
    "%-4s %s\n     ring %.0f ps (mean bin %.1f ps, calibrated bins %.0f..%.0f ps), delay pin->capture %.0f ps\n     single-edge error: %d events, rms %.1f ps (%.2f bins), worst %.1f ps (%.2f bins); per-class offsets: rms %.1f ps (%.2f bins), worst %.1f ps (%.2f bins); >3 bins: %d"
    corner (Capture_oracle.pp t.summary) t.period bin t.bin_min t.bin_max t.offset_ps t.samples
    t.rms_ps (t.rms_ps /. bin) t.worst_ps (t.worst_ps /. bin) t.rms_class_ps (t.rms_class_ps /. bin)
    t.worst_class_ps (t.worst_class_ps /. bin) t.outliers
;;

(* Random edges on all pins: gaps mostly 0..[mean_gap*2] ns, a fraction
   [near] of them 0..2 ns (near-coincident), deterministic. *)
let random_edges ?(min_same_pin_ns = 0.) ~seed ~count ~start_ns ~mean_gap_ns ~near () =
  let rng = Random.State.make [| seed |] in
  let last = Array.create ~len:Cfg.pins (-(Event_sim.of_ns 1e6)) in
  let min_same = Event_sim.of_ns min_same_pin_ns in
  let rec go acc t k =
    if k = 0
    then List.rev acc
    else (
      let gap =
        if Float.( < ) (Random.State.float rng 1.) near
        then Random.State.float rng 2.
        else Random.State.float rng (2. *. mean_gap_ns)
      in
      let t = t + Event_sim.of_ns gap + 1 in
      (* Pins toggle at most every [min_same_pin_ns] (pad limit). *)
      let ready = List.filter (List.init Cfg.pins ~f:Fn.id) ~f:(fun p -> t - last.(p) >= min_same) in
      match ready with
      | [] -> go acc t k
      | _ ->
        let pin = List.nth_exn ready (Random.State.int rng (List.length ready)) in
        last.(pin) <- t;
        go ((t, pin) :: acc) t (k - 1))
  in
  go [] (Event_sim.of_ns start_ns) count
;;

(* Pin snapshot race: for each isolated edge (no other edge within 3 ns),
   the time from the pin bit's new value reaching the loading slot's D (mux
   output) to that flop's clock edge. *)
let watch_race sim =
  let d = Hashtbl.Poly.create () in
  let c = Hashtbl.Poly.create () in
  List.iter [ "r"; "f" ] ~f:(fun bank ->
    for s = 0 to Cfg.slots - 1 do
      for i = 0 to Cfg.pins - 1 do
        let inst = Printf.sprintf "%s_%s_s%d_pins%d" name bank s i in
        Event_sim.on_change sim (inst ^ "_mux/X") (fun ~time _ ->
          Hashtbl.add_multi d ~key:(i, bank, s) ~data:time);
        Event_sim.on_change sim (inst ^ "/CLK") (fun ~time v ->
          if Event_sim.Value.equal v V1 then Hashtbl.add_multi c ~key:(i, bank, s) ~data:time)
      done
    done);
  d, c
;;

let race_margins (d, c) ~edges =
  let sorted = List.sort edges ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> Array.of_list in
  let isolated k =
    let t, _ = sorted.(k) in
    let w = Event_sim.of_ns 3. in
    (k = 0 || t - fst sorted.(k - 1) > w)
    && (k = Array.length sorted - 1 || fst sorted.(k + 1) - t > w)
  in
  let arr tbl key = Hashtbl.find tbl key |> Option.value ~default:[] |> List.sort ~compare:Int.compare |> Array.of_list in
  let cache = Hashtbl.Poly.create () in
  let get tbl tag key = Hashtbl.find_or_add cache (tag, key) ~default:(fun () -> arr tbl key) in
  let first_after a t =
    match Array.binary_search a ~compare:Int.compare `First_strictly_greater_than t with
    | Some i -> Some a.(i)
    | None -> None
  in
  let last_upto a t =
    match Array.binary_search a ~compare:Int.compare `Last_less_than_or_equal_to t with
    | Some i -> Some a.(i)
    | None -> None
  in
  List.filter_map (List.init (Array.length sorted) ~f:Fn.id) ~f:(fun k ->
    if not (isolated k)
    then None
    else (
      let t, pin = sorted.(k) in
      (* The clocked slot is the one whose D changed between the edge and
         its clock. *)
      List.find_map
        (List.cartesian_product [ "r"; "f" ] (List.init Cfg.slots ~f:Fn.id))
        ~f:(fun (bank, s) ->
          let key = pin, bank, s in
          match first_after (get c `C key) t with
          | Some tc when tc - t < Event_sim.of_ns 3. ->
            (match last_upto (get d `D key) tc with
             | Some td when td > t -> Some (tc - td)
             | _ -> None)
          | _ -> None)))
;;

(* Narrowest pulse on a bank clock leaf (minimum pulse width check). *)
let watch_pulses sim =
  let narrowest = ref Int.max_value in
  List.iter [ "r"; "f" ] ~f:(fun bank ->
    let last = ref None in
    Event_sim.on_change sim (Printf.sprintf "%s_clk%s_leaf0/X" name bank) (fun ~time _ ->
      (match !last with
       | Some t0 -> narrowest := Int.min !narrowest (time - t0)
       | None -> ());
      last := Some time));
  narrowest
;;

(* Collision sweep: pairs of edges on two different pins, [delta] apart,
   160 ns between pairs, random phase. Per delta: how many pairs came out as
   one event attributed to both pins, two separate events, or were reported
   without time / flagged. *)
type collision =
  { delta_ps : int
  ; merged : int
  ; separate : int
  ; flagged : int (* untimed, stale, or covered by a flag *)
  ; other : int
  }

let collision_sweep ~path ~deltas_ps ~reps ~seed =
  let rng = Random.State.make [| seed |] in
  let spacing = Event_sim.of_ns 160. in
  let pairs =
    List.concat_map deltas_ps ~f:(fun d -> List.init reps ~f:(fun k -> d, k))
    |> List.mapi ~f:(fun i (d, k) ->
      let t0 = Event_sim.of_ns 900. + (i * spacing) + Random.State.int rng (Event_sim.of_ns 20.) in
      let a = (i + k) % Cfg.pins in
      let b = (a + 1 + (k % (Cfg.pins - 1))) % Cfg.pins in
      d, t0, a, b)
  in
  let edges = List.concat_map pairs ~f:(fun (d, t0, a, b) -> [ t0, a; t0 + Event_sim.of_ps (Float.of_int d), b ]) in
  let cycles = ((List.last_exn pairs |> fun (_, t, _, _) -> t) / clock_period) + 450 in
  let r = simulate ~path ~edges ~cycles () in
  let t = analyse r ~edges in
  let flag_cycles =
    List.map r.pin_flags ~f:fst @ r.global_flags |> List.sort ~compare:Int.compare |> Array.of_list
  in
  let outcome (_, t0, a, b) =
    let c0 = t0 / clock_period in
    let evs = List.filter r.events ~f:(fun e -> e.cycle > c0 + 2 && e.cycle <= c0 + 11) in
    let bit p m = m land (1 lsl p) <> 0 in
    let timed = List.filter evs ~f:(fun e -> not e.untimed) in
    let any_flag =
      Array.exists flag_cycles ~f:(fun c -> c > c0 + 2 && c <= c0 + 11)
      || List.exists evs ~f:(fun e -> e.untimed || e.stale <> 0 || e.flagged)
    in
    if any_flag
    then `Flagged
    else (
      match timed with
      | [ e ] when bit a e.changed && bit b e.changed -> `Merged
      | [ e1; e2 ]
        when (bit a e1.changed && bit b e2.changed) || (bit b e1.changed && bit a e2.changed) ->
        `Separate
      | _ -> `Other)
  in
  let table =
    List.map deltas_ps ~f:(fun d ->
      let os = List.filter pairs ~f:(fun (d', _, _, _) -> d' = d) |> List.map ~f:outcome in
      let count x = List.count os ~f:(fun o -> Poly.equal o x) in
      { delta_ps = d
      ; merged = count `Merged
      ; separate = count `Separate
      ; flagged = count `Flagged
      ; other = count `Other
      })
  in
  table, t
;;
