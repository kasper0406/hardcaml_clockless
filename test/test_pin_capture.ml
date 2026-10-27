(* Pin_capture's drain, attribution and integrity checks in Cyclesim, with
   the behavioural front (Model) driven from timed edges by
   Pin_capture.Reference / Model_driver. Expectations come from the
   Reference (independent of the drain) and from Capture_oracle. *)

open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

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
  let status_width = 8
  let model_captures = 4
  let clock_leaf_fanout = 12
end

module Cap = Pin_capture.Make (Cfg) (R)
module Ref = Pin_capture.Reference

module Top = struct
  module I = struct
    type 'a t =
      { cap : 'a Cap.I.t [@rtlmangle "_"]
      ; stim : 'a Cap.Model_stimulus.t [@rtlmangle "_"]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) = Cap.create (Model i.stim) scope i.cap
end

module Sim = Cyclesim.With_interface (Top.I) (Cap.O)

let n = 17
let spc = 541 (* stage delays per clock cycle: ~20 ns at ~37 ps per stage *)
let modulus = (1 lsl R.count_width) * 2 * n
let all = (1 lsl Cfg.pins) - 1
let merge_window = 5
let swallow = 2

type out =
  { cycle : int
  ; ev : Bits.t Cap.Event.t
  }

type result =
  { outs : out list
  ; pin_flags : (int * int) list
  ; global_flags : int list
  ; status : Bits.t Cap.Status.t
  ; counts : int array
  (** counted from the [_now] pulses: fault overflow lost_edge level_mismatch
      merge_error empty stale runt *)
  ; ptr_resets : int
  ; cycles : int
  }

let b x = Bits.to_int_trunc x

(* Output cycle c is clock edge c, at stage time c * spc. *)
let cycle_time c = c * spc
let cycle_of_time t = (t + spc - 1) / spc

let run
  ?(enable = fun _ -> all)
  ?(trace = false)
  ?(corrupt = fun _ ~ptr_r:_ ~ptr_f:_ -> None)
  ?(initial = 0)
  ?(count_select = Cap.Count_source.Fault)
  ?(captures : Ref.capture list option)
  ~(edges : Ref.edge list)
  ~cycles
  ()
  =
  let captures =
    match captures with
    | Some c -> c
    | None -> fst (Ref.captures ~initial ~enabled:all ~merge_window ~swallow edges)
  in
  let scope = Scope.create ~flatten_design:true () in
  let sim =
    Sim.create
      ~config:(if trace then Cyclesim.Config.trace_all else Cyclesim.Config.default)
      (Top.create scope)
  in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let driver = Cap.Model_driver.create ~n ~stages_per_cycle:spc ~initial ~edges ~captures in
  let outs = ref [] in
  let pin_flags = ref [] in
  let global_flags = ref [] in
  let ptr_resets = ref 0 in
  let counts = Array.create ~len:8 0 in
  let model_ptr nm () =
    Cyclesim.lookup_reg_by_name sim nm |> Option.value_exn |> Cyclesim.Reg.to_bits |> Bits.to_int_trunc
  in
  for k = 0 to cycles - 1 do
    let c = k + 1 in
    i.cap.clear := Bits.of_bool (k < 2);
    i.cap.pin_enable := Bits.of_int_trunc ~width:Cfg.pins (enable c);
    i.cap.count_select
    := Bits.of_int_trunc ~width:Cap.Count_source.width (Cap.Count_source.code count_select);
    (match corrupt c ~ptr_r:(model_ptr "model_ptr_r") ~ptr_f:(model_ptr "model_ptr_f") with
     | Some (r, f) -> Cap.Model_driver.corrupt driver ~r ~f
     | None -> ());
    Cap.Model_driver.cycle driver i.stim;
    Cyclesim.cycle sim;
    let ev = Cap.Event.map o.event ~f:( ! ) in
    let st = Cap.Status.map o.status ~f:( ! ) in
    if Bits.to_bool ev.valid then outs := { cycle = c; ev } :: !outs;
    let pf = Bits.(to_int_trunc (st.lost_edge_now |: st.level_mismatch_now |: st.merge_error_now)) in
    if pf <> 0 then pin_flags := (c, pf) :: !pin_flags;
    if Bits.(to_bool (st.fault_now |: st.runt_now |: st.overflow_now |: st.resyncing)) && k >= 2
    then global_flags := c :: !global_flags;
    if Bits.to_bool st.ptr_reset then Int.incr ptr_resets;
    let add k x = counts.(k) <- counts.(k) + x in
    let pc x = Bits.(to_int_trunc (popcount x)) in
    add 0 (b st.fault_now);
    add 1 (b st.overflow_now);
    add 2 (pc st.lost_edge_now);
    add 3 (pc st.level_mismatch_now);
    add 4 (if b st.merge_error_now <> 0 then 1 else 0);
    add 5 (b st.empty_now);
    if Bits.to_bool ev.valid && not (Bits.to_bool ev.untimed) then add 6 (pc ev.stale);
    add 7 (b st.runt_now)
  done;
  { outs = List.rev !outs
  ; pin_flags = List.rev !pin_flags
  ; global_flags = List.rev !global_flags
  ; status = Cap.Status.map o.status ~f:( ! )
  ; counts
  ; ptr_resets = !ptr_resets
  ; cycles
  }
;;

let flagged (ev : Bits.t Cap.Event.t) = Bits.(to_bool (ev.fault |: ev.overflow |: ev.order_error |: ev.pattern_error))

(* Toggles to edges with levels. *)
let with_levels ~initial (edges : Ref.edge list) =
  let lv = ref initial in
  List.map (Ref.sort_edges edges) ~f:(fun (e : Ref.edge) ->
    lv := !lv lxor (1 lsl e.pin);
    { Capture_oracle.time = e.time; pin = e.pin; level = !lv land (1 lsl e.pin) <> 0 })
;;

(* The decoded ring time of a capture at stage time t is t + n - 1. *)
let est_of_time (ev : Bits.t Cap.Event.t) ~cycle =
  (* Unwrap the ring time against the output time: the capture happened
     within the last [modulus] stages before the output. *)
  let t_out = cycle_time cycle in
  let ring = b ev.time in
  let base = t_out - ((t_out - (ring - (n - 1))) % modulus) in
  base
;;

let oracle ?(initial = 0) ~edges (r : result) =
  Capture_oracle.check
    ~pins:Cfg.pins
    ~edges:(with_levels ~initial edges)
    ~events:
      (List.map r.outs ~f:(fun { cycle; ev } ->
         { Capture_oracle.cycle
         ; untimed = Bits.to_bool ev.untimed
         ; pins = b ev.pins
         ; changed = b ev.changed
         ; reconfig = b ev.reconfig
         ; stale = b ev.stale
         ; flagged = flagged ev
         ; est = (if Bits.to_bool ev.untimed then None else Some (est_of_time ev ~cycle))
         }))
    ~pin_flags:r.pin_flags
    ~global_flags:r.global_flags
    ~cycle_of_time
    ~cycle_time
    ~match_tol:merge_window
    ~back:4
    ~fwd:400
    ~end_cycle:r.cycles
;;

let print_status (s : Bits.t Cap.Status.t) (c : int array) =
  printf
    "status: fault %d (%d) overflow %d (%d) lost_edge %x (%d) level_mismatch %x (%d) merge_error %x (%d) empty %d stale %d resyncing %d\n"
    (b s.capture_fault) c.(0) (b s.overflow) c.(1) (b s.lost_edge)
    c.(2) (b s.level_mismatch) c.(3) (b s.merge_error)
    c.(4) c.(5) c.(6)
    (b s.resyncing)
;;

let print_events (r : result) =
  List.iter r.outs ~f:(fun { cycle; ev } ->
    printf
      "cycle %3d: %s pins %x changed %x reconfig %x stale %x time %5d%s%s%s\n"
      cycle
      (if Bits.to_bool ev.untimed then "untimed" else "timed  ")
      (b ev.pins) (b ev.changed) (b ev.reconfig) (b ev.stale)
      (if Bits.to_bool ev.untimed then 0 else b ev.time)
      (if Bits.to_bool ev.overflow then " OVERFLOW" else "")
      (if Bits.to_bool ev.fault then " FAULT" else "")
      (if Bits.to_bool ev.order_error then " ORDER" else ""))
;;

(* Random edges on [pins] pins from [start], gaps drawn from [gap]; at most
   [model_captures] edges per cycle. *)
let random_edges ~seed ~count ~start ~gap =
  let rng = Random.State.make [| seed |] in
  let rec go acc t k =
    if k = 0
    then List.rev acc
    else (
      let t = t + gap rng in
      let pin = Random.State.int rng Cfg.pins in
      go ({ Ref.time = t; pin } :: acc) t (k - 1))
  in
  let edges = go [] start count in
  (* Drop edges that would exceed the model's captures per cycle. *)
  let per_cycle = Hashtbl.create (module Int) in
  List.filter edges ~f:(fun e ->
    let c = cycle_of_time e.time in
    let k = Hashtbl.find per_cycle c |> Option.value ~default:0 in
    if k >= Cfg.model_captures
    then false
    else (
      Hashtbl.set per_cycle ~key:c ~data:(k + 1);
      true))
;;

(* --- 1. Exact agreement with the Reference ------------------------------ *)

let%expect_test "random edges: events equal the Reference's, times exact, zero unflagged errors" =
  (* ~1 edge per 4 cycles; 3% of gaps inside the merge window (but not so
     close that they cancel in the XOR). *)
  let gap rng =
    if Random.State.int rng 33 = 0
    then swallow + Random.State.int rng (merge_window + 2)
    else (2 * spc) + Random.State.int rng (6 * spc)
  in
  let edges = random_edges ~seed:1 ~count:4000 ~start:(30 * spc) ~gap in
  let cycles = cycle_of_time (List.last_exn edges).time + 200 in
  let r = run ~edges ~cycles () in
  let captures, swallowed = Ref.captures ~initial:0 ~enabled:all ~merge_window ~swallow edges in
  let expected = Ref.events ~initial:0 ~enabled:all captures in
  let timed = List.filter r.outs ~f:(fun o -> not (Bits.to_bool o.ev.untimed)) in
  let mismatches =
    if List.length timed <> List.length expected
    then -1
    else
      List.count (List.zip_exn timed expected) ~f:(fun (o, (e : Ref.event)) ->
        b o.ev.changed <> e.changed
        || b o.ev.stale <> e.stale
        || b o.ev.pins <> e.snapshot
        || b o.ev.time <> (e.time + n - 1) % modulus
        || flagged o.ev)
  in
  printf
    "edges %d, captures %d, cancelled toggles %d, reference events %d (%d multi-pin), timed events %d, mismatches %d\n"
    (List.length edges) (List.length captures) (List.length swallowed) (List.length expected)
    (List.count expected ~f:(fun e -> Int.popcount e.changed > 1))
    (List.length timed) mismatches;
  let sorted = Ref.sort_edges edges in
  let narrow =
    List.count (List.zip_exn (List.drop_last_exn sorted) (List.tl_exn sorted))
      ~f:(fun ((a : Ref.edge), (b : Ref.edge)) -> a.pin = b.pin && b.time - a.time <= merge_window)
  in
  printf "same-pin pulses within the merge window (both captures empty, lost_edge): %d\n" narrow;
  print_status r.status r.counts;
  print_endline (Capture_oracle.pp (oracle ~edges r));
  [%expect {|
    edges 4000, captures 4000, cancelled toggles 0, reference events 3911 (49 multi-pin), timed events 3911, mismatches 0
    same-pin pulses within the merge window (both captures empty, lost_edge): 19
    status: fault 0 (0) overflow 0 (0) lost_edge f (19) level_mismatch 0 (0) merge_error 0 (0) empty 89 stale 15 resyncing 0
    edges 4000: timed 3947, untimed 15, lost+flagged 38, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    |}]
;;

(* --- 2. Collisions ------------------------------------------------------- *)

let%expect_test "two pins delta apart: cancelled, merged into one event, or separate" =
  (* Model: toggles closer than [swallow] cancel; a snapshot includes edges
     up to [merge_window] after its trigger. *)
  printf "swallow %d, merge window %d stages\n" swallow merge_window;
  List.iter [ 0; 1; 2; 3; 5; 6; 12 ] ~f:(fun delta ->
    let edges =
      List.concat_map (List.init 8 ~f:Fn.id) ~f:(fun k ->
        let t = (40 + (12 * k)) * spc + 100 in
        [ { Ref.time = t; pin = 0 }; { Ref.time = t + delta; pin = 1 } ])
    in
    let r = run ~edges ~cycles:(40 + (12 * 8) + 60) () in
    let timed = List.filter r.outs ~f:(fun o -> not (Bits.to_bool o.ev.untimed)) in
    let untimed = List.filter r.outs ~f:(fun o -> Bits.to_bool o.ev.untimed) in
    let s = oracle ~edges r in
    printf
      "delta %2d: %2d timed events (%d attributed to both pins), %d untimed, empty %d, level_mismatch %d, merge_error %d | %s\n"
      delta (List.length timed)
      (List.count timed ~f:(fun o -> b o.ev.changed = 3))
      (List.length untimed) r.counts.(5) r.counts.(3)
      r.counts.(4) (Capture_oracle.pp s));
  [%expect {|
    swallow 2, merge window 5 stages
    delta  0:  0 timed events (0 attributed to both pins), 8 untimed, empty 0, level_mismatch 16, merge_error 0 | edges 16: timed 0, untimed 16, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    delta  1:  0 timed events (0 attributed to both pins), 8 untimed, empty 0, level_mismatch 16, merge_error 0 | edges 16: timed 0, untimed 16, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    delta  2:  8 timed events (8 attributed to both pins), 0 untimed, empty 8, level_mismatch 0, merge_error 0 | edges 16: timed 16, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    delta  3:  8 timed events (8 attributed to both pins), 0 untimed, empty 8, level_mismatch 0, merge_error 0 | edges 16: timed 16, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    delta  5:  8 timed events (8 attributed to both pins), 0 untimed, empty 8, level_mismatch 0, merge_error 0 | edges 16: timed 16, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    delta  6: 16 timed events (0 attributed to both pins), 0 untimed, empty 0, level_mismatch 0, merge_error 0 | edges 16: timed 16, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    delta 12: 16 timed events (0 attributed to both pins), 0 untimed, empty 0, level_mismatch 0, merge_error 0 | edges 16: timed 16, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    |}]
;;

(* --- 3. Overflow --------------------------------------------------------- *)

let%expect_test "bursts overflow the banks: flagged, resynchronised, nothing unflagged" =
  let gap rng =
    match Random.State.int rng 10 with
    | 0 -> 1 + Random.State.int rng 4
    | 1 | 2 | 3 -> Random.State.int rng (spc / 2)
    | _ -> Random.State.int rng (3 * spc)
  in
  let edges = random_edges ~seed:7 ~count:20000 ~start:(30 * spc) ~gap in
  let cycles = cycle_of_time (List.last_exn edges).time + 400 in
  let r = run ~edges ~cycles () in
  print_status r.status r.counts;
  let s = oracle ~edges r in
  print_endline (Capture_oracle.pp s);
  List.iter (List.take s.errors 10) ~f:print_endline;
  [%expect {|
    status: fault 0 (0) overflow 1 (3385) lost_edge f (72) level_mismatch f (106) merge_error d (2) empty 392 stale 1906 resyncing 0
    edges 19682: timed 3145, untimed 153, lost+flagged 16384, bad+flagged 5236 | UNFLAGGED: lost 0, phantom 0 | pending 0
    |}]
;;

let%expect_test "shared status counter: each source counts what its pulses report, saturating" =
  (* Two scenarios: bursts (overflow, stale, empty, lost edges), and gentle
     traffic with a corrupted pointer (fault). The counter is 8 bits wide
     here, so large counts saturate. Runts need the real cells (Event_sim);
     the Model front never produces them. *)
  let scenario ~seed ~count ~gap ~corrupt_at =
    let edges = random_edges ~seed ~count ~start:(30 * spc) ~gap in
    let cycles = cycle_of_time (List.last_exn edges).time + 400 in
    let corrupt c ~ptr_r ~ptr_f:_ =
      if c = corrupt_at then Some (ptr_r () lxor 0b010, 0) else None
    in
    List.iter Cap.Count_source.all ~f:(fun source ->
      let r = run ~trace:true ~corrupt ~count_select:source ~edges ~cycles () in
      let pulses = r.counts.(Cap.Count_source.code source) in
      let expect = Int.min pulses ((1 lsl Cfg.status_width) - 1) in
      let got = b r.status.count in
      printf "  %-15s pulses %4d counter %3d%s\n"
        (Sexp.to_string (Cap.Count_source.sexp_of_t source)) pulses got
        (if got = expect then "" else " MISMATCH"))
  in
  print_endline "bursts:";
  scenario ~seed:7 ~count:3000 ~corrupt_at:(-1) ~gap:(fun rng ->
    match Random.State.int rng 10 with
    | 0 -> 1 + Random.State.int rng 4
    | 1 | 2 | 3 -> Random.State.int rng (spc / 2)
    | _ -> Random.State.int rng (3 * spc));
  print_endline "gentle, pointer corrupted at cycle 300:";
  scenario ~seed:3 ~count:600 ~corrupt_at:300 ~gap:(fun rng -> spc + Random.State.int rng (4 * spc));
  [%expect {|
    bursts:
      Fault           pulses    0 counter   0
      Overflow        pulses  503 counter 255
      Lost_edge       pulses    9 counter   9
      Level_mismatch  pulses   21 counter  21
      Merge_error     pulses    1 counter   1
      Empty           pulses   54 counter  54
      Stale           pulses  242 counter 242
      Runt            pulses    0 counter   0
    gentle, pointer corrupted at cycle 300:
      Fault           pulses    1 counter   1
      Overflow        pulses    0 counter   0
      Lost_edge       pulses    0 counter   0
      Level_mismatch  pulses    0 counter   0
      Merge_error     pulses    0 counter   0
      Empty           pulses    0 counter   0
      Stale           pulses    0 counter   0
      Runt            pulses    0 counter   0
    |}]
;;

(* --- 4. Fault injection -------------------------------------------------- *)

let%expect_test "corrupted pointer: capture_fault, auto-reset, resync, then correct again" =
  let gap rng = spc + Random.State.int rng (4 * spc) in
  let edges = random_edges ~seed:3 ~count:600 ~start:(30 * spc) ~gap in
  let cycles = cycle_of_time (List.last_exn edges).time + 200 in
  (* Bank R's pointer becomes 010, an invalid Johnson code (not one of 000
     001 011 111 110 100), at cycle 300; bank F's jumps to a valid code one
     step ahead at cycle 700. *)
  let corrupt c ~ptr_r ~ptr_f =
    if c = 300
    then Some (ptr_r () lxor 0b010, 0)
    else if c = 700
    then (
      let p = ptr_f () in
      let idx = Pin_capture.Pointer.index_of_code ~slots:Cfg.slots p in
      Some (0, p lxor Pin_capture.Pointer.code_of_index ~slots:Cfg.slots (idx + 1)))
    else None
  in
  let r = run ~trace:true ~corrupt ~edges ~cycles () in
  print_status r.status r.counts;
  printf "pointer reset asserted for %d cycles\n" r.ptr_resets;
  let s = oracle ~edges r in
  print_endline (Capture_oracle.pp s);
  List.iter (List.take s.errors 10) ~f:print_endline;
  (* After recovery, events agree with the Reference again. *)
  let captures, _ = Ref.captures ~initial:0 ~enabled:all ~merge_window ~swallow edges in
  let expected =
    Ref.events ~initial:0 ~enabled:all captures
    |> List.filter ~f:(fun e -> e.time > 800 * spc)
  in
  let timed = List.filter r.outs ~f:(fun o -> not (Bits.to_bool o.ev.untimed)) in
  let late =
    match expected with
    | first :: _ ->
      List.drop_while timed ~f:(fun o -> b o.ev.time <> (first.time + n - 1) % modulus)
    | [] -> []
  in
  printf "after recovery: %d events, %d expected, mismatches %d\n" (List.length late)
    (List.length expected)
    (List.count (List.zip_exn (List.take late (List.length expected)) (List.take expected (List.length late)))
       ~f:(fun (o, (e : Ref.event)) ->
         b o.ev.changed <> e.changed || b o.ev.time <> (e.time + n - 1) % modulus || flagged o.ev));
  [%expect {|
    status: fault 1 (1) overflow 0 (0) lost_edge 9 (2) level_mismatch 9 (2) merge_error 0 (0) empty 0 stale 2 resyncing 0
    pointer reset asserted for 9 cycles
    edges 600: timed 594, untimed 1, lost+flagged 5, bad+flagged 5 | UNFLAGGED: lost 0, phantom 0 | pending 0
    after recovery: 344 events, 344 expected, mismatches 0
    |}]
;;

(* --- 5. Narrow pulses and cancellations ---------------------------------- *)

let%expect_test "a pulse narrower than the XOR: lost_edge on its pin, no event" =
  let edges =
    [ { Ref.time = (40 * spc) + 10; pin = 2 }
    ; { Ref.time = (40 * spc) + 11; pin = 2 } (* cancels *)
    ; { Ref.time = (60 * spc) + 10; pin = 1 }
    ; { Ref.time = (60 * spc) + 300; pin = 1 } (* separate events *)
    ]
  in
  let r = run ~edges ~cycles:500 () in
  print_events r;
  print_status r.status r.counts;
  print_endline (Capture_oracle.pp (oracle ~edges r));
  [%expect {|
    cycle  66: timed   pins 2 changed 2 reconfig 0 stale 0 time 15078
    cycle  67: timed   pins 0 changed 2 reconfig 0 stale 0 time 15368
    status: fault 0 (0) overflow 0 (0) lost_edge 4 (1) level_mismatch 0 (0) merge_error 0 (0) empty 0 stale 0 resyncing 0
    edges 4: timed 2, untimed 0, lost+flagged 2, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    |}]
;;

(* --- 6. Enable changes --------------------------------------------------- *)

let%expect_test "enabling and disabling a high pin creates no phantom edge" =
  (* Pin 3 goes high while disabled; enabled at cycle 60 (no toggle: the
     last snapshot already shows it high), disabled at 100, enabled again at
     140; pin 3 toggles at 80, at 120 (while disabled: not reported, and no
     capture refreshes the snapshot before the enable, so that capture shows
     it as [reconfig], not as an edge) and 170. Pin 0 toggles at 50, 90,
     180. *)
  let enable c = if c >= 60 && c < 100 || c >= 140 then all else 0b0111 in
  let edges =
    [ { Ref.time = (40 * spc) + 5; pin = 3 }
    ; { Ref.time = (50 * spc) + 5; pin = 0 }
    ; { Ref.time = (80 * spc) + 5; pin = 3 }
    ; { Ref.time = (90 * spc) + 5; pin = 0 }
    ; { Ref.time = (120 * spc) + 5; pin = 3 }
    ; { Ref.time = (170 * spc) + 5; pin = 3 }
    ; { Ref.time = (180 * spc) + 5; pin = 0 }
    ]
  in
  (* The front sees the enable two edges after the input changes (input
     register, applied register); the NAND toggles just after that edge. *)
  let enable_changes = [ (62 * spc) + 1, all; (102 * spc) + 1, 0b0111; (142 * spc) + 1, all ] in
  let captures, _ =
    Ref.captures ~enable_changes ~initial:0 ~enabled:0b0111 ~merge_window ~swallow edges
  in
  let r = run ~enable ~captures ~edges ~cycles:220 () in
  print_events r;
  print_status r.status r.counts;
  [%expect {|
    cycle  56: timed   pins 9 changed 1 reconfig 0 stale 0 time  9663
    cycle  86: timed   pins 1 changed 8 reconfig 0 stale 0 time  8485
    cycle  96: timed   pins 0 changed 1 reconfig 0 stale 0 time 13895
    cycle 148: timed   pins 8 changed 0 reconfig 8 stale 0 time  7207
    cycle 176: timed   pins 0 changed 8 reconfig 0 stale 0 time  4951
    cycle 186: timed   pins 1 changed 1 reconfig 0 stale 0 time 10361
    status: fault 0 (0) overflow 0 (0) lost_edge 0 (0) level_mismatch 0 (0) merge_error 0 (0) empty 1 stale 0 resyncing 0
    |}]
;;

(* --- 7. Cells mode: hierarchy, naming, protection ------------------------ *)

let%expect_test "cells mode: module protoemu_pin_capture, stable dont_touch instances" =
  let library = Cell_library.load "data/sg13cmos5l_excerpt.lib" in
  let dfrbpq : R.flop =
    { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
  in
  let scope = Scope.create ~flatten_design:false () in
  let module C = Circuit.With_interface (Cap.I) (Cap.O) in
  let circuit =
    C.create_exn
      ~name:"top"
      (Cap.hierarchical
         ~name:"cap"
         ~first_stage:(library, dfrbpq)
         ~module_name:"protoemu_pin_capture"
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
         scope)
  in
  let v =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
    |> Rtl.full_hierarchy |> Rope.to_string
  in
  let lines = String.split_lines v in
  printf "module protoemu_pin_capture present: %b\n"
    (String.is_substring v ~substring:"module protoemu_pin_capture");
  (* Census of hand-placed instances: attribute line, then the cell. *)
  let census = Hashtbl.create (module String) in
  List.iteri lines ~f:(fun k l ->
    if String.is_substring l ~substring:"dont_touch"
    then (
      let cell = String.strip (List.nth_exn lines (k + 1)) in
      Hashtbl.incr census cell));
  Hashtbl.to_alist census |> List.sort ~compare:[%compare: string * int]
  |> List.iter ~f:(fun (c, k) -> printf "%4d %s\n" k c);
  List.iter
    [ "cap_gate0"; "cap_xor2"; "cap_clkr"; "cap_clkf_leaf7"; "cap_r_ptr2"; "cap_f_s1_pins3"
    ; "cap_f_s1_pins3_mux"; "cap_r_s0_delayed0"; "cap_dly3_stage1"; "cap_ecf3"; "cap_sync_ptr_r0" ]
    ~f:(fun inst ->
      printf "%s: %b\n" inst
        (List.exists lines ~f:(fun l -> String.equal (String.strip l) inst)));
  (match Cap.hierarchical ~module_name:"x" (Model (Cap.Model_stimulus.Of_signal.wires ())) scope (Cap.I.Of_signal.wires ()) with
   | _ -> print_endline "Model accepted in a hierarchical scope"
   | exception e -> print_s [%sexp (e : exn)]);
  [%expect {|
    module protoemu_pin_capture present: true
      78 sg13cmos5l_buf_1
     213 sg13cmos5l_dfrbpq_1
       8 sg13cmos5l_dlygate4sd3_1
      15 sg13cmos5l_inv_1
     180 sg13cmos5l_mux2_1
       4 sg13cmos5l_nand2_1
       7 sg13cmos5l_xor2_1
    cap_gate0: true
    cap_xor2: true
    cap_clkr: true
    cap_clkf_leaf7: true
    cap_r_ptr2: true
    cap_f_s1_pins3: true
    cap_f_s1_pins3_mux: true
    cap_r_s0_delayed0: true
    cap_dly3_stage1: true
    cap_ecf3: true
    cap_sync_ptr_r0: true
    ("Pin_capture.Model is simulation-only: use it in a scope with ~flatten_design:true"
     (module_name x))
    |}]
;;

(* --- 8. Pins high at reset ----------------------------------------------- *)

let%expect_test "pins high at reset: edges counted forward, no false lost_edge" =
  (* The edge counter flops reset to 0 whatever the level, so a pin that is
     high then counts its first edge on the falling-edge flop. Pins 0 and 2
     start high; each pin toggles 3 times, checked after every edge. *)
  let initial = 0b0101 in
  let edges = List.init 12 ~f:(fun k -> { Ref.time = ((40 + (10 * k)) * spc) + 7; pin = k % 4 }) in
  let r = run ~initial ~edges ~cycles:200 () in
  print_events r;
  print_status r.status r.counts;
  print_endline (Capture_oracle.pp (oracle ~initial ~edges r));
  [%expect {|
    cycle  46: timed   pins 4 changed 1 reconfig 0 stale 0 time  4255
    cycle  56: timed   pins 6 changed 2 reconfig 0 stale 0 time  9665
    cycle  66: timed   pins 2 changed 4 reconfig 0 stale 0 time 15075
    cycle  76: timed   pins a changed 8 reconfig 0 stale 0 time  3077
    cycle  86: timed   pins b changed 1 reconfig 0 stale 0 time  8487
    cycle  96: timed   pins 9 changed 2 reconfig 0 stale 0 time 13897
    cycle 106: timed   pins d changed 4 reconfig 0 stale 0 time  1899
    cycle 116: timed   pins 5 changed 8 reconfig 0 stale 0 time  7309
    cycle 126: timed   pins 4 changed 1 reconfig 0 stale 0 time 12719
    cycle 136: timed   pins 6 changed 2 reconfig 0 stale 0 time   721
    cycle 146: timed   pins 2 changed 4 reconfig 0 stale 0 time  6131
    cycle 156: timed   pins a changed 8 reconfig 0 stale 0 time 11541
    status: fault 0 (0) overflow 0 (0) lost_edge 0 (0) level_mismatch 0 (0) merge_error 0 (0) empty 0 stale 0 resyncing 0
    edges 12: timed 12, untimed 0, lost+flagged 0, bad+flagged 0 | UNFLAGGED: lost 0, phantom 0 | pending 0
    |}]
;;

(* --- 9. Capture balance over many reads ---------------------------------- *)

let%expect_test "capture balance far below 0 between checks: merge_error, not wrapped" =
  (* Each cycle, pins 0+1 and 2+3 toggle in cancelling pairs, then pin 0
     again: one capture attributing pins 1-3 (balance -2). The levels change
     every cycle, so there is no balance check until the end: -18 after 9
     cycles. Pin 0 loses 18 edges (2 mod 4: lost_edge). *)
  let edges =
    List.concat_map (List.init 9 ~f:Fn.id) ~f:(fun g ->
      let t = ((40 + g) * spc) + 100 in
      List.map [ 0, 0; 1, 1; 3, 2; 4, 3; 7, 0 ] ~f:(fun (dt, pin) -> { Ref.time = t + dt; pin }))
  in
  let r = run ~edges ~cycles:500 () in
  print_status r.status r.counts;
  print_endline (Capture_oracle.pp (oracle ~edges r));
  [%expect {|
    status: fault 0 (0) overflow 0 (0) lost_edge 1 (1) level_mismatch 0 (0) merge_error e (1) empty 0 stale 0 resyncing 0
    edges 45: timed 18, untimed 0, lost+flagged 27, bad+flagged 9 | UNFLAGGED: lost 0, phantom 0 | pending 0
    |}]
;;
