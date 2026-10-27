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

(* The baseline for reported numbers: the library's default wire load
   ("1k"), pre-layout. [Flat] (1 fF per net) is kept for regressions. *)
let baseline_wire_load = Event_sim.Config.pre_layout.wire_load

let wire_load_name (wire_load : Event_sim.Config.wire_load) path =
  match wire_load with
  | Flat -> "flat 1 fF"
  | Liberty_default ->
    Printf.sprintf "Liberty %s" (Timing_library.default_wire_load (Timing_library.load path)).name
  | _ -> "other"
;;

(* L and E measured for both samplers of this ring (the clk sampler and the
   capture slots) by test_pin_capture_timing.ml. *)
module R = Ring.Make_timed (struct
    let lengths = [ 17 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 9
    let counter_earliest_change_stages = 4
  end)

module Make (Slots : sig
    val slots : int
  end) =
struct
  module Cfg = struct
    let pins = 4
    let slots = Slots.slots
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
        ; count_select = Signal.zero Cap.Count_source.width
        ; ring = state
        ; ring_time = sampled.time
        ; ring_time_valid = Signal.(sampled.valid &: ~:(sampled.pattern_error))
        }
    in
    Circuit.create_exn
      ~name:"capture_timing"
      (Signal.output "node0" (Signal.bit state.nodes ~pos:0)
       :: Signal.output "smp_valid" sampled.valid
       :: Signal.output "smp_phase" sampled.phase
       :: Cap.O.(to_list (map2 port_names o ~f:Signal.output)))
  ;;

  let config ?(seed = 0) ?(wire_load = baseline_wire_load) () =
    { Event_sim.Config.pre_layout with seed; wire_load }
  ;;

  (* One run: [edges] are (time fs, pin) toggles from the levels [initial].
     Samples the outputs every cycle (mid-cycle, after the edge they were
     registered on). *)
  type event =
    { cycle : int
    ; untimed : bool
    ; pins : int
    ; changed : int
    ; reconfig : int
    ; stale : int
    ; flagged : bool
    ; loss_mark : bool (* [lost]: the D-13 mark after a resync *)
    ; fault : bool (* torn, or read just before a capture fault *)
    ; overflow : bool
    ; implausible : bool
    ; bank : int
    ; time : int
    ; phase : int
    ; periods : int
    ; age_cycles : float (* plausibility check: ring time from capture to read, in cycles *)
    }

  type run =
    { events : event list
    ; flags : Capture_oracle.flags
    ; status : (string * int) list
    ; rises : int array (* node0 rising edges *)
    ; sim : Event_sim.t
    ; cycles : int
    ; clk_phases : int list (* clk sampler phases, for calibration *)
    ; x_outputs : int (* X on a flag or event_valid after start-up *)
    ; torn : int list (* cycles of [status_torn_now] *)
    ; ptr_resets : int list (* cycles of [status_ptr_reset] *)
    }

  let status_names =
    [ "capture_fault"; "fault_count"; "overflow"; "overflow_count"; "lost_edge"; "lost_edge_count"
    ; "level_mismatch"; "level_mismatch_count"; "merge_error"; "merge_error_count"; "empty_count"
    ; "stale_count"; "runt_count"; "torn_count"; "implausible"; "implausible_count"
    ; "resyncing"
    ]
  ;;

  let get sim nm = Option.value (Event_sim.to_int sim nm) ~default:(-1)

  (* A simulation of the circuit, started: ring enabled at 5 ns, drain
     cleared for 3 cycles, every pin enabled at [initial]. *)
  let start ?(seed = 0) ?wire_load ?config:cfg ?(initial = 0) ~path () =
    let tl = Timing_library.load path in
    let cfg = match cfg with Some c -> c | None -> config ~seed ?wire_load () in
    let sim = Event_sim.create ~config:cfg tl (circuit tl.library) in
    Event_sim.set_input_int sim "enable" 0 ~at:0;
    Event_sim.set_input_int sim "enable" 1 ~at:(Event_sim.of_ns 5.);
    Event_sim.set_input_int sim "clear" 1 ~at:0;
    Event_sim.set_input_int sim "clear" 0 ~at:(3 * clock_period);
    Event_sim.set_input_int sim "pins" initial ~at:0;
    Event_sim.set_input_int sim "pin_enable" ((1 lsl Cfg.pins) - 1) ~at:0;
    Event_sim.add_clock sim "clock" ~period:clock_period;
    tl, sim
  ;;

  let simulate ?seed ?wire_load ?(initial = 0) ?(setup = fun (_ : Event_sim.t) -> ()) ~path ~(edges : (int * int) list) ~cycles () =
    let _, sim = start ?seed ?wire_load ~initial ~path () in
    let level = Array.init Cfg.pins ~f:(fun p -> (initial lsr p) land 1) in
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
    let faults = Queue.create () in
    let resync = Queue.create () in
    let runts = Queue.create () in
    let clk_phases = Queue.create () in
    let x_outputs = ref 0 in
    let torn = Queue.create () in
    let ptr_resets = Queue.create () in
    (* Status counts, from the [_now] pulses and the events. *)
    let counts = Hashtbl.create (module String) in
    let add nm x = if x > 0 then Hashtbl.update counts nm ~f:(fun o -> Option.value o ~default:0 + x) in
    let pc nm = Int.popcount (Int.max 0 (get sim nm)) in
    for c = 1 to cycles do
      Event_sim.run_until sim ((c * clock_period) + (clock_period / 2));
      if get sim "smp_valid" = 1 && c > 10 then Queue.enqueue clk_phases (get sim "smp_phase");
      add "fault_count" (pc "status_fault_now");
      add "overflow_count" (pc "status_overflow_now");
      add "lost_edge_count" (pc "status_lost_edge_now");
      add "level_mismatch_count" (pc "status_level_mismatch_now");
      add "merge_error_count" (if get sim "status_merge_error_now" > 0 then 1 else 0);
      add "empty_count" (pc "status_empty_now");
      add "runt_count" (pc "status_runt_now");
      add "torn_count" (pc "status_torn_now");
      if get sim "status_torn_now" = 1 then Queue.enqueue torn c;
      if get sim "status_ptr_reset" = 1 then Queue.enqueue ptr_resets c;
      add "implausible_count" (pc "status_implausible_now");
      if get sim "event_valid" = 1 && get sim "event_untimed" = 0 then add "stale_count" (pc "event_stale");
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
              (let fl =
                 List.map [ "fault"; "overflow"; "order_error"; "pattern_error"; "implausible" ] ~f:g
               in
               (* an unknown flag is an X output, not a flag *)
               if List.exists fl ~f:(fun v -> v < 0) then Int.incr x_outputs;
               List.exists fl ~f:(fun v -> v > 0))
          ; loss_mark = g "lost" > 0
          ; fault = g "fault" > 0
          ; overflow = g "overflow" > 0
          ; implausible = g "implausible" > 0
          ; bank = g "bank"
          ; time = g "time"
          ; phase = g "phase"
          ; periods = g "periods"
          ; age_cycles =
              (let step = try get sim "plaus_step" with _ -> 0 in
               if step <= 0 then Float.nan else Float.of_int (get sim "plaus_age") /. Float.of_int step)
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
           || flag "status_implausible_now" <> 0
        then Queue.enqueue faults c;
        if flag "status_resyncing" <> 0 then Queue.enqueue resync c;
        if flag "status_runt_now" <> 0 then Queue.enqueue runts c;
        if get sim "event_valid" < 0 then Int.incr x_outputs)
    done;
    { events = Queue.to_list events
    ; flags =
        { pin = Queue.to_list pin_flags
        ; faults = Queue.to_list faults
        ; resync = Queue.to_list resync
        ; runts = Queue.to_list runts
        }
    ; status =
        List.map status_names ~f:(fun nm ->
          if String.is_suffix nm ~suffix:"_count"
          then nm, Option.value (Hashtbl.find counts nm) ~default:0
          else nm, get sim ("status_" ^ nm))
    ; rises = Queue.to_array rises
    ; sim
    ; cycles
    ; clk_phases = Queue.to_list clk_phases
    ; x_outputs = !x_outputs
    ; torn = Queue.to_list torn
    ; ptr_resets = Queue.to_list ptr_resets
    }
  ;;

  (* ---- Analysis ------------------------------------------------------------ *)

  let range_periods = 1 lsl R.count_width
  let bins = 2 * n

  (* The oracle's coverage contract (Capture_oracle), in cycles, from the
     drain's structure: a record waits at most for the other unread records
     (2 slots per bank) plus synchronisation before it is read, and leaves
     the pipeline 3 cycles later. With no edge for [quiet_cycles], the
     pointers and levels are synchronised and stable, the FIFO is drained and
     the level and count checks have run. *)
  let windows =
    { Capture_oracle.in_flight = 4
    ; back = 4
    ; before = (2 * Cfg.slots) + 4
    ; after = 5
    ; event_age = (2 * Cfg.slots) + 8
    }
  ;;

  let quiet_cycles = (2 * Cfg.slots) + 6

  (* Mean ring period (fs) from node0's rising edges. *)
  let period_fs (r : run) =
    let xs = Array.sub r.rises ~pos:10 ~len:(Array.length r.rises - 10) in
    Float.of_int (xs.(Array.length xs - 1) - xs.(0)) /. Float.of_int (Array.length xs - 1)
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
    let cycle_of_time t = (t + clock_period - 1) / clock_period in
    let deadline =
      Capture_oracle.deadlines ~quiet_cycles ~end_cycle:r.cycles
        (List.map edges ~f:(fun (t, _) -> cycle_of_time t))
    in
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
           ; loss_mark = e.loss_mark
           ; est = (if e.untimed then None else est e)
           }))
      ~flags:r.flags
      ~cycle_of_time
      ~cycle_time:(fun c -> c * clock_period)
      ~match_tol
      ~windows
      ~deadline
      ~end_cycle:r.cycles
  ;;

  (* Calibration, as fractions of the ring period so that it carries over to
     another run at the same corner: code-density bin widths, the constant
     delay from the pin to the capture (edge time minus estimate), and an
     offset per (pin, polarity, bank) on top of it. *)
  type calibration =
    { width : float array
    ; start : float array
    ; offset : float (* fs *)
    ; class_offset : ((int * bool * int) * float) list (* fs *)
    }

  let calibrated_bins phases =
    let hist = Array.create ~len:bins 0 in
    List.iter phases ~f:(fun p -> if p >= 0 && p < bins then hist.(p) <- hist.(p) + 1);
    let total = Float.of_int (List.length phases) in
    let width = Array.map hist ~f:(fun h -> Float.of_int h /. total) in
    let start = Array.create ~len:bins 0. in
    for p = 1 to bins - 1 do
      start.(p) <- start.(p - 1) +. width.(p - 1)
    done;
    width, start
  ;;

  let estimate_with (r : run) (cal : calibration) ~k_offset =
    let period = period_fs r in
    let width = Array.map cal.width ~f:(fun w -> w *. period) in
    let start = Array.map cal.start ~f:(fun s -> s *. period) in
    fun (e : event) ->
      (* a single-pin event's class is known before matching: correct it
         before validation, so the validated times are the reported ones *)
      let class_offset =
        if Int.popcount e.changed = 1
        then (
          let pin = Int.ctz e.changed in
          List.Assoc.find cal.class_offset (pin, e.pins land (1 lsl pin) <> 0, e.bank) ~equal:Poly.equal
          |> Option.value ~default:0.)
        else 0.
      in
      Option.map (estimate r ~width ~start ~k_offset e) ~f:(fun t ->
        t + Float.iround_nearest_exn (cal.offset +. class_offset))
  ;;

  (* The counters' start offset (the enable race) is per run: the one whose
     estimates match the most edges to within a third of a ring period (a
     wrong offset is a whole period off). *)
  let best_k_offset (r : run) cal ~edges =
    let tol = Float.iround_nearest_exn (period_fs r /. 3.) in
    List.map (List.range (-4) 5) ~f:(fun k_offset ->
      let s = oracle r ~edges ~est:(estimate_with r cal ~k_offset) ~match_tol:tol in
      s.timed, k_offset)
    |> List.max_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> Option.value_exn
    |> snd
  ;;

  let match_tol = Event_sim.of_ns 2.
  let cls (edge : Capture_oracle.edge) (e : event) = edge.pin, edge.level, e.bank

  (* Residuals (edge time minus estimate) of the validated attributions of
     a summary, by number of pins in the event. *)
  let residuals (r : run) (s : Capture_oracle.summary) =
    let by_cycle = Hashtbl.create (module Int) in
    List.iter r.events ~f:(fun e -> Hashtbl.set by_cycle ~key:e.cycle ~data:e);
    List.filter_map s.matches ~f:(fun (ev, edge, k) ->
      Option.map ev.est ~f:(fun t ->
        k, cls edge (Hashtbl.find_exn by_cycle ev.cycle), Float.of_int (edge.time - t)))
  ;;

  (* Fit a calibration to a run: bins from its captures' phases, then the
     offsets from its single-pin attributions. *)
  let fit (r : run) ~edges =
    let width, start = calibrated_bins (List.filter_map r.events ~f:(fun e -> Option.some_if (not e.untimed) e.phase)) in
    let cal0 = { width; start; offset = 0.; class_offset = [] } in
    let median xs =
      let a = List.sort xs ~compare:Float.compare |> Array.of_list in
      if Array.is_empty a then 0. else a.(Array.length a / 2)
    in
    (* The delay from pin to capture is unknown here (it can exceed a ring
       period): per counter offset, its median at a wide tolerance; then the
       pair that matches best. *)
    let cal1, k_offset =
      List.map [ -2; -1; 0; 1; 2 ] ~f:(fun k_offset ->
        let wide =
          oracle r ~edges ~est:(estimate_with r cal0 ~k_offset) ~match_tol:(Event_sim.of_ns 3.)
        in
        let offset =
          median (List.filter_map (residuals r wide) ~f:(fun (k, _, x) -> Option.some_if (k = 1) x))
        in
        { cal0 with offset }, k_offset)
      |> List.max_elt ~compare:(fun (c1, k1) (c2, k2) ->
        let score c k =
          (oracle r ~edges ~est:(estimate_with r c ~k_offset:k)
             ~match_tol:(Float.iround_nearest_exn (period_fs r /. 3.))).timed
        in
        Int.compare (score c1 k1) (score c2 k2))
      |> Option.value_exn
    in
    let singles =
      residuals r (oracle r ~edges ~est:(estimate_with r cal1 ~k_offset) ~match_tol)
      |> List.filter_map ~f:(fun (k, c, x) -> Option.some_if (k = 1) (c, x))
    in
    let mean xs = List.sum (module Float) xs ~f:Fn.id /. Float.of_int (Int.max 1 (List.length xs)) in
    let m = mean (List.map singles ~f:snd) in
    let class_offset =
      List.sort_and_group singles ~compare:(fun (a, _) (b, _) -> Poly.compare a b)
      |> List.map ~f:(fun g -> fst (List.hd_exn g), mean (List.map g ~f:snd) -. m)
    in
    (* The delay is only known modulo the ring period (the counter offset
       absorbs whole periods): keep it in (-P/2, P/2] so that a run with
       another counter start finds its offset in [best_k_offset]'s range. *)
    let p = period_fs r in
    let offset = cal1.offset +. m in
    let offset = offset -. (p *. Float.round_nearest (offset /. p)) in
    { cal1 with offset; class_offset }
  ;;

  type timing =
    { summary : Capture_oracle.summary
    ; period : float (* ps *)
    ; bin_min : float
    ; bin_max : float
    ; offset_ps : float
    ; singles : float list (* ps, residuals of validated single-pin attributions *)
    ; multi : float list (* ps, validated attributions of multi-pin events *)
    ; k_offset : int
    }

  (* Validate a run against a calibration (by default fitted to the same
     run, so in-sample). *)
  let analyse ?calibration (r : run) ~edges =
    let cal = match calibration with Some c -> c | None -> fit r ~edges in
    let k_offset = best_k_offset r cal ~edges in
    let summary = oracle r ~edges ~est:(estimate_with r cal ~k_offset) ~match_tol in
    let res = residuals r summary |> List.map ~f:(fun (k, _, x) -> k, x /. 1000.) in
    let period = period_fs r in
    let ps x = x /. 1000. in
    { summary
    ; period = ps period
    ; bin_min = ps (period *. Array.fold cal.width ~init:Float.infinity ~f:Float.min)
    ; bin_max = ps (period *. Array.fold cal.width ~init:0. ~f:Float.max)
    ; offset_ps = ps cal.offset
    ; singles = List.filter_map res ~f:(fun (k, x) -> Option.some_if (k = 1) x)
    ; multi = List.filter_map res ~f:(fun (k, x) -> Option.some_if (k > 1) x)
    ; k_offset
    }
  ;;

  (* The torn-record and plausibility flags against the true edges: how
     many there were, how many implausible events carried no other flag,
     and how many flagged records decoded to a right time anyway (each
     changed pin has an edge of that level within [match_tol] of the
     estimate: a conservative flag). Also the plausibility ages (ring time
     from capture to the check, in clock cycles) of the unflagged timed
     events. *)
  type flag_audit =
    { torn : int
    ; torn_right : int
    ; implausible : int
    ; implausible_alone : int
    ; implausible_alone_right : int
    ; max_age : float
    ; ages : float list
    }

  let flag_audit (r : run) ~edges ~est =
    let lv = with_levels edges |> Array.of_list in
    let times = Array.map lv ~f:(fun (e : Capture_oracle.edge) -> e.time) in
    let right (e : event) =
      match est e with
      | None -> false
      | Some x ->
        List.for_all (List.init Cfg.pins ~f:Fn.id) ~f:(fun p ->
          e.changed land (1 lsl p) = 0
          ||
          let lo =
            Array.binary_search times ~compare:Int.compare `First_greater_than_or_equal_to (x - match_tol)
            |> Option.value ~default:(Array.length times)
          in
          let rec go k =
            k < Array.length lv
            && lv.(k).time <= x + match_tol
            && ((lv.(k).pin = p && Bool.equal lv.(k).level (e.pins land (1 lsl p) <> 0)) || go (k + 1))
          in
          go lo)
    in
    let torn_cycles = Set.of_list (module Int) r.torn in
    let timed = List.filter r.events ~f:(fun e -> not e.untimed) in
    let torn = List.filter timed ~f:(fun e -> e.fault && Set.mem torn_cycles (e.cycle - 2)) in
    let impl = List.filter timed ~f:(fun e -> e.implausible) in
    let alone = List.filter impl ~f:(fun e -> not (e.fault || e.overflow)) in
    let ages =
      List.filter_map timed ~f:(fun e ->
        Option.some_if ((not e.flagged) && not (Float.is_nan e.age_cycles)) e.age_cycles)
    in
    { torn = List.length torn
    ; torn_right = List.count torn ~f:right
    ; implausible = List.length impl
    ; implausible_alone = List.length alone
    ; implausible_alone_right = List.count alone ~f:right
    ; max_age = List.fold ages ~init:0. ~f:Float.max
    ; ages
    }
  ;;

  let rms xs =
    Float.sqrt (List.sum (module Float) xs ~f:(fun x -> x *. x) /. Float.of_int (Int.max 1 (List.length xs)))
  ;;

  let worst xs = List.fold xs ~init:0. ~f:(fun a x -> Float.max a (Float.abs x))

  let quantile xs q =
    let a = List.map xs ~f:Float.abs |> List.sort ~compare:Float.compare |> Array.of_list in
    if Array.is_empty a
    then 0.
    else a.(Int.max 0 (Float.iround_up_exn (q *. Float.of_int (Array.length a)) - 1))
  ;;

  (* Timing, separately from accounting: how many edges were validated
     against their true time, and the error distribution of those. *)
  let pp_timing_stats ~bin ~edges ~no_estimate singles multi =
    let n_s = List.length singles
    and n_m = List.length multi in
    Printf.sprintf
      "timing: %d of %d edges validated within 2 ns (%.1f%%), %d timed attributions without an estimate\n     single-pin events: %d, rms %.1f ps (%.2f bins), p99 %.1f ps, worst %.1f ps (%.2f bins), >3 bins %d, >0.5 ns %d, >half a ring period %d\n     multi-pin events: %d attributions, rms %.1f ps, worst %.1f ps"
      (n_s + n_m) edges
      (100. *. Float.of_int (n_s + n_m) /. Float.of_int (Int.max 1 edges))
      no_estimate n_s (rms singles) (rms singles /. bin) (quantile singles 0.99) (worst singles)
      (worst singles /. bin)
      (List.count singles ~f:(fun x -> Float.(abs x > 3. *. bin)))
      (List.count singles ~f:(fun x -> Float.(abs x > 500.)))
      (* a whole period off still matches within 2 ns at fast corners *)
      (List.count singles ~f:(fun x -> Float.(abs x > bin *. of_int bins /. 2.)))
      n_m (rms multi) (worst multi)
  ;;

  let pp_latency (s : Capture_oracle.summary) =
    Printf.sprintf
      "per-pin flag latency: %d errors covered, max %d cycles (deadline: end of the first %d-cycle quiet stretch)"
      (List.length s.latencies)
      (List.fold s.latencies ~init:0 ~f:Int.max)
      quiet_cycles
  ;;

  let pp_timing ~corner ?(calibrated = "on this run") t =
    let bin = t.period /. Float.of_int bins in
    Printf.sprintf
      "%-4s %s\n     ring %.0f ps (mean bin %.1f ps, calibrated %s: bins %.0f..%.0f ps), delay pin->capture %.0f ps\n     %s\n     %s"
      corner (Capture_oracle.pp t.summary) t.period bin calibrated t.bin_min t.bin_max
      (* the delay is known modulo the ring period *)
      (Float.mod_float t.offset_ps t.period -. if Float.(Float.mod_float t.offset_ps t.period > 0.) then t.period else 0.)
      (pp_timing_stats ~bin ~edges:t.summary.edges ~no_estimate:t.summary.no_estimate t.singles t.multi)
      (pp_latency t.summary)
    ^ "\n     "
    ^ Capture_oracle.pp_resyncs t.summary
  ;;

  (* Edge-to-event latency of the matched, unflagged, timed single-pin
     attributions (not untimed, merged or flagged events), in clock edges: the first rising clock edge at or after the pin edge is 1, and
     the event is on the outputs (valid) after that many edges. All events,
     and those whose edge had no other edge on any pin within [quiet_ns]
     (no backlog in the FIFO, no burst). *)
  let latencies ?(quiet_ns = 100.) (t : timing) ~edges =
    let times = List.map edges ~f:fst |> List.sort ~compare:Int.compare |> Array.of_list in
    let quiet = Event_sim.of_ns quiet_ns in
    let isolated time =
      (* the edge itself is the only one within [quiet] *)
      let near =
        match
          ( Array.binary_search times ~compare:Int.compare `First_greater_than_or_equal_to (time - quiet)
          , Array.binary_search times ~compare:Int.compare `Last_less_than_or_equal_to (time + quiet) )
        with
        | Some lo, Some hi -> hi - lo + 1
        | _ -> 0
      in
      near = 1
    in
    let all =
      List.filter_map t.summary.matches ~f:(fun (ev, (edge : Capture_oracle.edge), k) ->
        Option.some_if
          (k = 1)
          (edge.time, ev.cycle - ((edge.time + clock_period - 1) / clock_period) + 1))
    in
    List.map all ~f:snd, List.filter_map all ~f:(fun (time, l) -> Option.some_if (isolated time) l)
  ;;

  let pp_latencies xs =
    let a = List.sort xs ~compare:Int.compare |> Array.of_list in
    let n = Array.length a in
    if n = 0
    then "none"
    else
      Printf.sprintf "%d events, min %d, median %d, p99 %d, max %d; %s" n a.(0) a.(n / 2)
        a.(Int.max 0 (((n * 99) + 99) / 100 - 1)) a.(n - 1)
        (List.sort_and_group (Array.to_list a) ~compare:Int.compare
         |> List.map ~f:(fun g -> Printf.sprintf "%d:%d" (List.hd_exn g) (List.length g))
         |> String.concat ~sep:" ")
  ;;

  (* Random edges on all pins: gaps mostly 0..[mean_gap*2] ns, a fraction
     [near] of them 0..2 ns (near-coincident), deterministic. After every
     [quiet_every] edges, [quiet_ns] without edges, so that the drain's
     checks run and flag deadlines are bounded (Capture_oracle). *)
  let random_edges
    ?(min_same_pin_ns = 0.)
    ?(quiet_every = Int.max_value)
    ?(quiet_ns = 400.)
    ~seed
    ~count
    ~start_ns
    ~mean_gap_ns
    ~near
    ()
    =
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
        let gap = if (count - k) % quiet_every = 0 && k < count then gap +. quiet_ns else gap in
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

  let race_margins ?(ptr_resets = []) (d, c) ~edges =
    let sorted = List.sort edges ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> Array.of_list in
    (* a pointer reset moves the slot selects whenever it comes: not the
       pin's race *)
    let resets = Set.of_list (module Int) ptr_resets in
    let isolated k =
      let t, _ = sorted.(k) in
      let w = Event_sim.of_ns 3. in
      let c = t / clock_period in
      (k = 0 || t - fst sorted.(k - 1) > w)
      && (k = Array.length sorted - 1 || fst sorted.(k + 1) - t > w)
      && not (List.exists [ c - 1; c; c + 1; c + 2 ] ~f:(Set.mem resets))
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

  let collision_sweep ?wire_load ~path ~deltas_ps ~reps ~seed () =
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
    let r = simulate ?wire_load ~path ~edges ~cycles () in
    let t = analyse r ~edges in
    let flag_cycles =
      List.map r.flags.pin ~f:fst @ r.flags.faults @ r.flags.resync @ r.flags.runts
      |> List.sort ~compare:Int.compare
      |> Array.of_list
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


  (* Same-bank write spacing sweep: triples of edges on three different
     pins, [d] apart first to third (the second halfway), so that one bank
     is written twice [d] apart. Triples are [spacing_ns] apart, so the slot
     the second write lands in was last written that long ago: a record torn
     between its old and new contents decodes far off. Per [d]: triples
     whose third edge's event was flagged torn, and the events whose
     decoded time matches no true edge of their pins (from {!flag_audit}'s
     rule), flagged and unflagged. *)
  type burst_row =
    { d_ps : int
    ; torn : int
    ; wrong_flagged : int
    ; wrong_unflagged : int
    }

  let burst_sweep ?wire_load ?(spacing_ns = 1000.) ~calibration ~path ~ds_ps ~reps ~seed () =
    let rng = Random.State.make [| seed |] in
    let triples =
      List.concat_map ds_ps ~f:(fun d -> List.init reps ~f:(fun k -> d, k))
      |> List.mapi ~f:(fun i (d, k) ->
        let t0 =
          Event_sim.of_ns (1300. +. (spacing_ns *. Float.of_int i))
          + Random.State.int rng (Event_sim.of_ns 20.)
        in
        let a = (i + k) % Cfg.pins in
        d, t0, [ a; (a + 1) % Cfg.pins; (a + 2) % Cfg.pins ])
    in
    (* Isolated edges first, so that every slot holds a record (not the
       flops' initial X). *)
    let prime =
      List.init (4 * Cfg.slots) ~f:(fun k -> Event_sim.of_ns (300. +. (100. *. Float.of_int k)), k % Cfg.pins)
    in
    let edges =
      prime
      @ List.concat_map triples ~f:(fun (d, t0, pins) ->
        List.mapi pins ~f:(fun j p -> t0 + (Event_sim.of_ps (Float.of_int d) * j / 2), p))
    in
    let cycles = (List.last_exn edges |> fst) / clock_period + 100 in
    let r = simulate ?wire_load ~path ~edges ~cycles () in
    let t = analyse ~calibration r ~edges in
    let est = estimate_with r calibration ~k_offset:t.k_offset in
    let lv = with_levels edges in
    let right (e : event) =
      match est e with
      | None -> false
      | Some x ->
        List.for_all (List.init Cfg.pins ~f:Fn.id) ~f:(fun p ->
          e.changed land (1 lsl p) = 0
          || List.exists lv ~f:(fun (ed : Capture_oracle.edge) ->
            ed.pin = p
            && Bool.equal ed.level (e.pins land (1 lsl p) <> 0)
            && abs (ed.time - x) <= match_tol))
    in
    let torn_cycles = Set.of_list (module Int) r.torn in
    let rows =
      List.map ds_ps ~f:(fun d ->
        let mine = List.filter triples ~f:(fun (d', _, _) -> d' = d) in
        let evs =
          List.filter r.events ~f:(fun e ->
            (not e.untimed)
            && e.changed <> 0
            && List.exists mine ~f:(fun (_, t0, _) ->
              let c0 = t0 / clock_period in
              e.cycle > c0 && e.cycle <= c0 + 20))
        in
        let wrong = List.filter evs ~f:(fun e -> not (right e)) in
        { d_ps = d
        ; torn = List.count evs ~f:(fun e -> e.fault && Set.mem torn_cycles (e.cycle - 2))
        ; wrong_flagged = List.count wrong ~f:(fun e -> e.flagged)
        ; wrong_unflagged = List.count wrong ~f:(fun e -> not e.flagged)
        })
    in
    rows, t, r
  ;;

  (* ---- Counter-selection margins of both samplers ------------------------ *)

  (* The capture slots sample the ring through a data buffer and the slot's
     mux, [D = sel ? fresh : Q]: while a slot is selected (from one write to
     its bank to the next), its D pins follow the ring. So with the pins
     quiet for [quiet] the selected slot of each bank shows the ring at its D
     pins; then one rise and one fall of pin 0 write each bank once and
     select the next slot, for every slot in turn. [quiet] covers a full
     counter cycle at the slow corner, so every counter bit changes both
     ways in every window (checked).

     Each record's flops hang off several clock leaves: a D transition is
     moved into the bank trunk's frame by subtracting that flop's clock
     arrival after the trunk (measured at the writes; the spread over writes
     is reported). Apertures come from the flop's tables at the D slew and
     its own clock slew. At each write the pointer moves the selects: the
     written slot's mux turns to Q and the next slot's to the ring. Both
     changes must come after the hold time of the flops just clocked; after
     the quiet windows, more writes take each bank's pointer through all its
     states, and the smallest signed margin over every write is reported
     (a select change missing where one is due raises). The clk sampler is
     measured in the same run (here the ring also drives the capture's data
     buffers). *)
  type slot_margin =
    { label : string
    ; w : Ring_margins.wavefront
    ; a : Ring_margins.counter
    ; b : Ring_margins.counter
    ; skew_ps : float (* spread of clock arrival over the record's flops *)
    ; skew_jitter_ps : float (* largest change of one flop's arrival between writes *)
    ; d_delay_ps : float * float (* ring net -> slot D, min/max over node bits *)
    ; handoff_ps : float (* select change at a write, after CLK + hold, signed *)
    ; ordered : bool (* the wavefront at the D pins, in the trunk frame, is in order *)
    }

  let flop_cell = "sg13cmos5l_dfrbpq_1"

  let sampler_margins ?wire_load ?(quiet = Event_sim.of_ns 2400.) ~path () =
    let tl, sim = start ?wire_load ~path () in
    let first = Event_sim.of_ns 300. in
    let gap = Event_sim.of_ns 200. in
    let window k = first + (k * (quiet + gap)) in
    let in_window t =
      List.find (List.init Cfg.slots ~f:Fn.id) ~f:(fun k -> t >= window k && t < window k + quiet)
    in
    (* One write per bank after each window, then more, 200 ns apart, until
       each pointer has been through all its states. *)
    let write_pair t =
      Event_sim.set_input sim "pins[0]" V1 ~at:(t + Event_sim.of_ns 40.);
      Event_sim.set_input sim "pins[0]" V0 ~at:(t + Event_sim.of_ns 120.)
    in
    for k = 0 to Cfg.slots - 1 do
      write_pair (window k + quiet)
    done;
    for k = Cfg.slots to Cap.ptr_range - 1 do
      write_pair (window Cfg.slots + ((k - Cfg.slots) * gap))
    done;
    let end_time = window Cfg.slots + ((Cap.ptr_range - Cfg.slots) * gap) in
    let rising name = function
      | Event_sim.Value.V1 -> true
      | V0 -> false
      | X -> raise_s [%message "X after warm-up" (name : string)]
    in
    let warm t = t >= Event_sim.of_ns 200. in
    (* The ring nets and counters, with the clk sampler's apertures. *)
    let raw_nodes = Queue.create () in
    let clk_changes = [| Queue.create (); Queue.create () |] in
    let clk_record name ~f =
      Event_sim.on_change sim name (fun ~time v ->
        if warm time
        then (
          let r = rising name v in
          let setup, hold =
            Ring_margins.aperture tl ~cell:flop_cell ~rising:r ~d_slew:(Event_sim.slew_ns sim name)
              ~clock_slew:(Event_sim.slew_ns sim "clock")
          in
          f { Ring_margins.time; setup; hold } r))
    in
    for i = 0 to n - 1 do
      clk_record (Printf.sprintf "ring_node%d" i) ~f:(fun t r -> Queue.enqueue raw_nodes (i, r, t))
    done;
    List.iteri [ "a"; "b" ] ~f:(fun c tag ->
      for i = 0 to R.counter_width - 1 do
        clk_record (Printf.sprintf "ring_count_%s[%d]" tag i) ~f:(fun t _ ->
          Queue.enqueue clk_changes.(c) t)
      done);
    (* Bank trunks: clock rises. *)
    let trunk = Hashtbl.create (module String) in
    List.iter [ "r", "clkr/X"; "f", "clkf/Y" ] ~f:(fun (bank, pin) ->
      Event_sim.on_change sim (Printf.sprintf "%s_%s" name pin) (fun ~time v ->
        if Event_sim.Value.equal v V1 && warm time then Hashtbl.add_multi trunk ~key:bank ~data:time));
    (* Slot flops: D transitions (time, rising, slew) in the window of their
       slot; clock rises (time, slew); select changes. *)
    let fields = [ "nodes", n; "count_a", R.counter_width; "count_b", R.counter_width ] in
    let d = Hashtbl.Poly.create () in
    let clk = Hashtbl.Poly.create () in
    let sel = Hashtbl.Poly.create () in
    List.iter [ "r"; "f" ] ~f:(fun bank ->
      for slot = 0 to Cfg.slots - 1 do
        List.iter fields ~f:(fun (field, width) ->
          for i = 0 to width - 1 do
            let key = bank, slot, field, i in
            let inst = Printf.sprintf "%s_%s_s%d_%s%d" name bank slot field i in
            Event_sim.on_change sim (inst ^ "_mux/X") (fun ~time v ->
              match in_window time with
              | Some k when k = slot ->
                Hashtbl.add_multi d ~key ~data:(time, rising inst v, Event_sim.slew_ns sim (inst ^ "_mux/X"))
              | _ -> ());
            Event_sim.on_change sim (inst ^ "/CLK") (fun ~time v ->
              if Event_sim.Value.equal v V1 && warm time
              then Hashtbl.add_multi clk ~key ~data:(time, Event_sim.slew_ns sim (inst ^ "/CLK")));
            Event_sim.on_change sim (inst ^ "_mux/S") (fun ~time _ ->
              if warm time then Hashtbl.add_multi sel ~key ~data:time)
          done)
      done);
    Event_sim.run_until sim end_time;
    let raw = Ring_margins.of_node_transitions ~n (Queue.to_list raw_nodes) in
    let s = R.select_threshold n in
    let clk_sampler =
      { label = "clk sampler"
      ; w = raw
      ; a = Ring_margins.counter raw ~first:0 ~s (Queue.to_list clk_changes.(0))
      ; b = Ring_margins.counter raw ~first:n ~s (Queue.to_list clk_changes.(1))
      ; skew_ps = 0.
      ; skew_jitter_ps = 0.
      ; d_delay_ps = 0., 0.
      ; handoff_ps = Float.nan
      ; ordered = true
      }
    in
    (* Raw transitions of node i: (time, wavefront index). *)
    let raw_of_node =
      Array.init n ~f:(fun i ->
        Array.filter_mapi raw.tr ~f:(fun q (t : Ring_margins.transition) ->
          Option.some_if (q % n = i) (t.time, q)))
    in
    let slot_margin bank slot =
      let trunk_rises = Hashtbl.find_exn trunk bank |> List.sort ~compare:Int.compare |> Array.of_list in
      if Array.length trunk_rises <> Cap.ptr_range
      then raise_s [%message "expected one write per pointer state" bank (Array.length trunk_rises : int)];
      let handoff = ref Int.max_value in
      (* Per flop: clock arrival after the trunk (mean, and its spread over
         writes) and clock slew; the select handoff after each write. *)
      let clock_of ((_, _, field, i) as key) =
        let flop_hold clock_slew =
          List.map [ true; false ] ~f:(fun rising ->
            snd (Ring_margins.aperture tl ~cell:flop_cell ~rising ~d_slew:0.05 ~clock_slew))
          |> List.fold ~init:Int.min_value ~f:Int.max
        in
        let rises = Hashtbl.find_exn clk key |> List.sort ~compare:Poly.compare |> Array.of_list in
        let sels = Hashtbl.find_multi sel key |> List.sort ~compare:Int.compare |> Array.of_list in
        (* Write [w] (the trunk's [w]-th rise) writes slot [w mod slots] and
           selects the next: both change their select. *)
        Array.iteri trunk_rises ~f:(fun w tw ->
          let clk, clock_slew =
            Array.find rises ~f:(fun (t, _) -> t >= tw && t - tw < Event_sim.of_ns 2.)
            |> Option.value_exn ~message:"no clock edge for a write"
          in
          let due = slot = w % Cfg.slots || slot = (w + 1) % Cfg.slots in
          match Array.find sels ~f:(fun t -> t > tw - Event_sim.of_ns 1. && t - tw < Event_sim.of_ns 5.) with
          | Some t -> handoff := Int.min !handoff (t - clk - flop_hold clock_slew)
          | None ->
            if due
            then raise_s [%message "no select change at a write" bank (slot : int) field (i : int) (w : int)]);
        let rises = Array.to_list rises in
        let offsets =
          List.map rises ~f:(fun (t, _) ->
            let k =
              Array.binary_search trunk_rises ~compare:Int.compare `Last_less_than_or_equal_to t
              |> Option.value_exn
            in
            t - trunk_rises.(k))
        in
        let mean = List.sum (module Int) offsets ~f:Fn.id / List.length offsets in
        let spread = List.fold offsets ~init:0 ~f:(fun a o -> Int.max a (abs (o - mean))) in
        let slew = List.sum (module Float) rises ~f:snd /. Float.of_int (List.length rises) in
        mean, spread, slew
      in
      let transitions key =
        let offset, spread, clock_slew = clock_of key in
        ( List.map (Hashtbl.find_multi d key) ~f:(fun (time, r, d_slew) ->
            let setup, hold =
              Ring_margins.aperture tl ~cell:flop_cell ~rising:r ~d_slew ~clock_slew
            in
            time, r, { Ring_margins.time = time - offset; setup; hold })
        , offset
        , spread )
      in
      (* Node bits: each D transition follows the latest transition of its
         ring node, whose wavefront index it takes. *)
      let indexed = Hashtbl.create (module Int) in
      let delays = ref [] in
      let offsets = ref [] in
      let spreads = ref [] in
      for i = 0 to n - 1 do
        let ts, offset, spread = transitions (bank, slot, "nodes", i) in
        offsets := offset :: !offsets;
        spreads := spread :: !spreads;
        List.iter ts ~f:(fun (time, _, t) ->
          match
            Array.binary_search raw_of_node.(i) ~compare:(fun (x, _) t -> Int.compare x t)
              `Last_less_than_or_equal_to time
          with
          | Some k ->
            let raw_time, q = raw_of_node.(i).(k) in
            delays := (time - raw_time) :: !delays;
            if Hashtbl.mem indexed q then raise_s [%message "two D transitions for one node transition" (q : int)];
            Hashtbl.set indexed ~key:q ~data:t
          | None -> ())
      done;
      (* The run of consecutive indices from the first rise of node 0. *)
      let qs = Hashtbl.keys indexed |> List.sort ~compare:Int.compare in
      let q0 = List.find_exn qs ~f:(fun q -> q % (2 * n) = 0 && Hashtbl.mem indexed (q + 1)) in
      let rec last q = if Hashtbl.mem indexed (q + 1) then last (q + 1) else q in
      let q1 = last q0 in
      if q1 - q0 + 1 < List.length qs - (2 * n)
      then raise_s [%message "gaps in the D-pin wavefront" (q0 : int) (q1 : int)];
      let tr = Array.init (q1 - q0 + 1) ~f:(fun k -> Hashtbl.find_exn indexed (q0 + k)) in
      let w = Ring_margins.wavefront ~n tr in
      let ordered =
        Array.for_alli tr ~f:(fun k (t : Ring_margins.transition) -> k = 0 || t.time > tr.(k - 1).time)
      in
      let changes field =
        List.concat_map (List.init R.counter_width ~f:Fn.id) ~f:(fun i ->
          let ts, offset, spread = transitions (bank, slot, field, i) in
          offsets := offset :: !offsets;
          spreads := spread :: !spreads;
          (* every bit must change both ways in the window *)
          if not (List.exists ts ~f:(fun (_, r, _) -> r) && List.exists ts ~f:(fun (_, r, _) -> not r))
          then raise_s [%message "counter bit did not toggle in the window" bank (slot : int) field (i : int)];
          List.map ts ~f:(fun (_, _, t) -> t))
      in
      let ps x = Event_sim.to_ps x in
      let fold f init l = List.fold l ~init ~f in
      let a = Ring_margins.counter w ~first:0 ~s (changes "count_a") in
      let b = Ring_margins.counter w ~first:n ~s (changes "count_b") in
      { label = Printf.sprintf "bank %s slot %d" bank slot
      ; w
      ; a
      ; b
      ; skew_ps = ps (fold Int.max Int.min_value !offsets - fold Int.min Int.max_value !offsets)
      ; skew_jitter_ps = ps (fold Int.max 0 !spreads)
      ; d_delay_ps = ps (fold Int.min Int.max_value !delays), ps (fold Int.max 0 !delays)
      ; handoff_ps = ps !handoff
      ; ordered
      }
    in
    clk_sampler
    :: List.concat_map [ "r"; "f" ] ~f:(fun bank ->
      List.init Cfg.slots ~f:(fun slot -> slot_margin bank slot))
  ;;

  (* ---- Tag lag (Pin_capture's torn-record check) ----------------------- *)

  (* At every select change of a slot (on: its muxes turn to the fresh
     values; off: back to the flops' own Q), the tag bits' D must change
     after every other bit's D by more than the flop aperture: the data
     bit's setup plus the tag bit's hold. Times are moved into each flop's
     clock frame (its clock arrival after the bank trunk, measured at the
     writes), so clock skew within the record is included. Isolated edges
     (one write per merged toggle, [gap_ns] apart) on random pins vary the
     data; [writes] merged toggles in all. Per bank and direction: the
     smallest margin (lag minus aperture) and the number of select
     changes. *)
  type tag_margin =
    { bank : string
    ; on_ps : float
    ; off_ps : float
    ; lag_on_ps : float (* smallest lag, for reference *)
    ; lag_off_ps : float
    ; changes : int
    }

  let tag_margins ?wire_load ?config ?(writes = 48) ?(gap_ns = 150.) ~path () =
    let tl, sim = start ?wire_load ?config ~path () in
    let rng = Random.State.make [| 3 |] in
    let level = Array.create ~len:Cfg.pins false in
    for k = 0 to writes - 1 do
      let p = Random.State.int rng Cfg.pins in
      level.(p) <- not level.(p);
      Event_sim.set_input sim (Printf.sprintf "pins[%d]" p) (if level.(p) then V1 else V0)
        ~at:(Event_sim.of_ns (400. +. (gap_ns *. Float.of_int k) +. Random.State.float rng 20.))
    done;
    let end_time = Event_sim.of_ns (600. +. (gap_ns *. Float.of_int writes)) in
    let warm t = t >= Event_sim.of_ns 300. in
    let fields =
      [ "nodes", n; "count_a", R.counter_width; "count_b", R.counter_width; "pins", Cfg.pins
      ; "delayed", Cfg.pins ]
    in
    let bits = List.concat_map fields ~f:(fun (f, w) -> List.init w ~f:(fun i -> f, i, false)) in
    let bits = bits @ List.init Cap.ptr_bits ~f:(fun i -> "tag", i, true) in
    (* Front_cells' select polarity: the slot loads fresh values when its
       select is high, except slot 0 (and with 2 slots, slot 0 of the
       shared parity net). *)
    let loads_high slot = slot <> 0 in
    let trunk = Hashtbl.create (module String) in
    List.iter [ "r", "clkr/X"; "f", "clkf/Y" ] ~f:(fun (bank, pin) ->
      Event_sim.on_change sim (Printf.sprintf "%s_%s" name pin) (fun ~time v ->
        if Event_sim.Value.equal v V1 && warm time then Hashtbl.add_multi trunk ~key:bank ~data:time));
    let s_changes = Hashtbl.Poly.create () in
    let x_changes = Hashtbl.Poly.create () in
    let clk = Hashtbl.Poly.create () in
    List.iter [ "r"; "f" ] ~f:(fun bank ->
      for slot = 0 to Cfg.slots - 1 do
        List.iter bits ~f:(fun (f, i, _) ->
          let key = bank, slot, f, i in
          let inst = Printf.sprintf "%s_%s_s%d_%s%d" name bank slot f i in
          (* only select changes that change the mux output: the newly
             selected input differs from the old one *)
          Event_sim.on_change sim (inst ^ "_mux/S") (fun ~time v ->
            let a0 = Event_sim.value sim (inst ^ "_mux/A0")
            and a1 = Event_sim.value sim (inst ^ "_mux/A1") in
            if warm time && not (Event_sim.Value.equal a0 a1)
            then Hashtbl.add_multi s_changes ~key ~data:(time, v));
          Event_sim.on_change sim (inst ^ "_mux/X") (fun ~time v ->
            if warm time
            then
              Hashtbl.add_multi x_changes ~key
                ~data:(time, Event_sim.Value.equal v V1, Event_sim.slew_ns sim (inst ^ "_mux/X")));
          Event_sim.on_change sim (inst ^ "/CLK") (fun ~time v ->
            if Event_sim.Value.equal v V1 && warm time
            then Hashtbl.add_multi clk ~key ~data:(time, Event_sim.slew_ns sim (inst ^ "/CLK"))))
      done);
    Event_sim.run_until sim end_time;
    let sorted l = List.sort l ~compare:Poly.compare |> Array.of_list in
    List.map [ "r"; "f" ] ~f:(fun bank ->
      let trunk_rises = Hashtbl.find_multi trunk bank |> List.sort ~compare:Int.compare |> Array.of_list in
      (* Per flop: clock arrival after the trunk (mean over writes) and slew. *)
      let clock_of key =
        let rises = Hashtbl.find_multi clk key |> sorted in
        let offs =
          Array.to_list rises
          |> List.filter_map ~f:(fun (t, _) ->
            Array.binary_search trunk_rises ~compare:Int.compare `Last_less_than_or_equal_to t
            |> Option.map ~f:(fun k -> t - trunk_rises.(k)))
        in
        ( List.sum (module Int) offs ~f:Fn.id / Int.max 1 (List.length offs)
        , Array.fold rises ~init:0. ~f:(fun a (_, sl) -> a +. sl) /. Float.of_int (Int.max 1 (Array.length rises)) )
      in
      (* Select-induced D transitions, in the flop's clock frame, keyed by
         the write that preceded the select change. *)
      let events = Hashtbl.Poly.create () in
      for slot = 0 to Cfg.slots - 1 do
        List.iter bits ~f:(fun (f, i, is_tag) ->
          let key = bank, slot, f, i in
          let off, clock_slew = clock_of key in
          let xs = Hashtbl.find_multi x_changes key |> sorted in
          List.iter (Hashtbl.find_multi s_changes key) ~f:(fun (ts, v) ->
            match
              Array.binary_search trunk_rises ~compare:Int.compare `Last_less_than_or_equal_to ts
            with
            | None -> ()
            | Some w ->
              let on = Bool.equal (Event_sim.Value.equal v V1) (loads_high slot) in
              (match
                 Array.find xs ~f:(fun (tx, _, _) -> tx > ts && tx - ts < Event_sim.of_ns 0.6)
               with
               | None -> ()
               | Some (tx, rising, d_slew) ->
                 let setup, hold =
                   Ring_margins.aperture tl ~cell:flop_cell ~rising ~d_slew ~clock_slew
                 in
                 Hashtbl.add_multi events ~key:(w, slot, on) ~data:(is_tag, tx - off, setup, hold)))
        )
      done;
      let ps x = Event_sim.to_ps x in
      (* lag: earliest tag D minus latest data D; margin: the earliest
         tag D less its hold, minus the latest data D plus its setup, each
         taken over all bits *)
      let margins =
        Hashtbl.to_alist events
        |> List.filter_map ~f:(fun ((_, _, on), xs) ->
          let data = List.filter xs ~f:(fun (t, _, _, _) -> not t) in
          let tags = List.filter xs ~f:(fun (t, _, _, _) -> t) in
          let fold l f init g = List.fold l ~init ~f:(fun a x -> f a (g x)) in
          if List.is_empty data || List.is_empty tags
          then None
          else (
            let td = fold data Int.max Int.min_value (fun (_, t, _, _) -> t) in
            let tt = fold tags Int.min Int.max_value (fun (_, t, _, _) -> t) in
            let data_req = fold data Int.max Int.min_value (fun (_, t, setup, _) -> t + setup) in
            let tag_req = fold tags Int.min Int.max_value (fun (_, t, _, hold) -> t - hold) in
            Some (on, ps (tt - td), ps (tag_req - data_req))))
      in
      let worst on f =
        List.filter margins ~f:(fun (o, _, _) -> Bool.equal o on)
        |> List.fold ~init:Float.infinity ~f:(fun a m -> Float.min a (f m))
      in
      { bank
      ; on_ps = worst true (fun (_, _, m) -> m)
      ; off_ps = worst false (fun (_, _, m) -> m)
      ; lag_on_ps = worst true (fun (_, l, _) -> l)
      ; lag_off_ps = worst false (fun (_, l, _) -> l)
      ; changes = List.length margins
      })
  ;;

  let pp_tag_margin m =
    Printf.sprintf
      "bank %s: %d select changes; tag D after every data D by >= %.0f ps (on) / %.0f ps (off); margin over setup + hold >= %.0f / %.0f ps"
      m.bank m.changes m.lag_on_ps m.lag_off_ps m.on_ps m.off_ps
  ;;

  let pp_sampler_margin m =
    let s = R.select_threshold n in
    let lo, hi = m.d_delay_ps in
    Printf.sprintf
      "%-14s %s%s"
      m.label
      (Ring_margins.pp_counters m.w ~s m.a m.b)
      (if String.equal m.label "clk sampler"
       then ""
       else
         Printf.sprintf
           "\n                    ring -> D %.0f..%.0f ps, clock skew in the record %.0f ps (+-%.0f between writes), select handoff %.0f ps after hold, wavefront in order: %b"
           lo hi m.skew_ps m.skew_jitter_ps m.handoff_ps m.ordered)
  ;;

  (* Whether a set of samplers meets the asserted bounds: margins of at
     least [min_stages], in-order wavefronts, select handoffs after hold. *)
  let samplers_ok ~min_stages ms =
    List.for_all ms ~f:(fun m ->
      Float.(Ring_margins.worst m.a m.b >= min_stages)
      && m.ordered
      && (String.equal m.label "clk sampler" || Float.(m.handoff_ps > 0.)))
  ;;

  (* One line for a set of samplers: the phases safe for all of them, the
     smallest margin and handoff, the largest change of a clock arrival. *)
  let pp_margin_summary ms =
    let fold f init g = List.fold ms ~init ~f:(fun acc m -> f acc (g m)) in
    let slots = List.filter ms ~f:(fun m -> not (String.equal m.label "clk sampler")) in
    let s = R.select_threshold n in
    Printf.sprintf
      "%d samplers: safe phases A [%d, %d%+d) B [%d, %d%+d), s=%d, smallest margin %.1f stages; select handoff >= %.0f ps after hold, wavefronts in order: %b, clock arrival +-%.0f ps between writes"
      (List.length ms)
      (fold Int.max 0 (fun m -> m.a.safe_from))
      (2 * n)
      (fold Int.min Int.max_value (fun m -> m.a.safe_until))
      (fold Int.max 0 (fun m -> m.b.safe_from))
      (2 * n)
      (fold Int.min Int.max_value (fun m -> m.b.safe_until))
      s
      (fold Float.min Float.infinity (fun m -> Ring_margins.worst m.a m.b))
      (List.fold slots ~init:Float.infinity ~f:(fun a m -> Float.min a m.handoff_ps))
      (List.for_all ms ~f:(fun m -> m.ordered))
      (List.fold slots ~init:0. ~f:(fun a m -> Float.max a m.skew_jitter_ps))
  ;;
end

include Make (struct
    let slots = 2
  end)
