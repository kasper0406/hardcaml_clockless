(* Tests of [Tdc].

   The Model-mode tests compare the RTL against two oracles written here,
   independently of the RTL:
   - [Wave]: the physics. The input is a list of edge times in units of one
     stage delay, and position [i] sampled at time [t] shows the input level at
     [t - i]. From the edge times alone it predicts the sampled vector and the
     event a correct TDC reports (fine = how long before the clock edge the
     edge arrived).
   - [reference_decode]: the decoding rules of tdc.ml (majority filter, first
     run, window), restated over OCaml arrays. Used where the physics only
     bounds the answer (bubbles).

   Sample [n] is taken at time [n * stages_per_cycle], at the [n]-th
   [Cyclesim.cycle] (counting from 0, clear cycles included). *)

open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

let lib = lazy (Cell_library.load "data/sg13cmos5l_excerpt.lib")

module Event = struct
  type t =
    { valid : bool
    ; rising : bool
    ; fine : int
    ; coarse : int
    ; overflow : bool
    ; multi_edge : bool
    }
  [@@deriving sexp_of, equal]

  let none =
    { valid = false; rising = false; fine = 0; coarse = 0; overflow = false; multi_edge = false }
  ;;

  let is_none t = equal t none
end

(* ---- Oracle 1: physics ---- *)

module Wave = struct
  type t =
    { stages_per_cycle : int
    ; edges : int array (** strictly increasing, all >= 1; the input starts low *)
    }

  let sample_time t n = n * t.stages_per_cycle

  (** Input level at [time]: high iff an odd number of edges happened at or
      before it. *)
  let level t time =
    (* number of edges <= time, by binary search *)
    let lo = ref 0
    and hi = ref (Array.length t.edges) in
    while !lo < !hi do
      let mid = (!lo + !hi) / 2 in
      if t.edges.(mid) <= time then lo := mid + 1 else hi := mid
    done;
    !lo % 2 = 1
  ;;

  (** Edges in the cycle that ends with sample [n]: times in (T(n-1), T(n)]. *)
  let edges_in_cycle t n =
    Array.filter t.edges ~f:(fun a ->
      a > sample_time t (n - 1) && a <= sample_time t n)
    |> Array.to_list
  ;;

  let sampled t ~length n = Array.init (length + 1) ~f:(fun i -> level t (sample_time t n - i))

  (** What a correct TDC reports for sample [n] (bubble-free sampling, edges at
      least 2 stages apart). *)
  let expected t ~length ~window ~coarse n : Event.t =
    let now = sample_time t n in
    match edges_in_cycle t n with
    | [] -> Event.none
    | [ a ] ->
      let k = now - a in
      { valid = true
      ; rising = level t now
      ; fine = Int.min k length
      ; coarse
      ; overflow = k >= length
      ; multi_edge = false
      }
    | [ a1; a2 ] ->
      (* A pulse: out at [k], back at [j]. Positions j+1..k show it. It is
         seen if it covers 2 positions inside the line (narrower ones are
         filtered) and starts inside the window. *)
      let k = now - a1
      and j = now - a2 in
      if k - j >= 2 && j + 2 <= length && j + 1 <= window - 1
      then
        { valid = false
        ; rising = level t now
        ; fine = j
        ; coarse
        ; overflow = false
        ; multi_edge = true
        }
      else Event.none
    | _ -> failwith "Wave: more than two edges in a cycle"
  ;;

  (** Random edges, at least [min_gap] apart, at most [per_cycle] per cycle,
      none before [start] or after [stop]. *)
  let random rng ~stages_per_cycle ~min_gap ~max_gap ~per_cycle ~start ~stop =
    let cycle_of a = (a + stages_per_cycle - 1) / stages_per_cycle in
    let rec go t acc =
      if t > stop
      then { stages_per_cycle; edges = Array.of_list (List.rev acc) }
      else (
        let in_cycle = List.count acc ~f:(fun a -> cycle_of a = cycle_of t) in
        if in_cycle >= per_cycle
        then go (cycle_of t * stages_per_cycle + 1) acc
        else go (t + min_gap + Random.State.int rng (max_gap - min_gap + 1)) (t :: acc))
    in
    go start []
  ;;
end

(* ---- Oracle 2: the decoding rules, restated ---- *)

let reference_decode ~length ~window ~level ~coarse (s : bool array) : Event.t =
  let n = length + 1 in
  let d j = j < n && Bool.( <> ) s.(j) level in
  let m =
    Array.init n ~f:(fun j ->
      if j = 0 then d 0 else List.count [ d (j - 1); d j; d (j + 1) ] ~f:Fn.id >= 2)
  in
  let edge = m.(0) in
  (* length of the initial run *)
  let run =
    Array.findi m ~f:(fun _ b -> Bool.( <> ) b edge)
    |> Option.value_map ~default:n ~f:fst
  in
  (* a 0 -> 1 step inside the window means a second edge *)
  let multi_edge =
    List.exists (List.range 0 (window - 1)) ~f:(fun j -> (not m.(j)) && m.(j + 1))
  in
  if edge || multi_edge
  then
    { valid = edge
    ; rising = s.(0)
    ; fine = run - 1
    ; coarse
    ; overflow = edge && run = n
    ; multi_edge
    }
  else Event.none
;;

(* ---- Harness ---- *)

(* Samples 0 and 1 are taken with clear high (the default); sample [n0] is
   the first one reported, with coarse 0. *)
let n0 = 2

module Harness (T : Tdc.S) = struct
  module I = struct
    type 'a t =
      { tdc : 'a T.I.t
      ; stim : 'a T.Model_stimulus.t [@rtlprefix "stim_"]
      }
    [@@deriving hardcaml]
  end

  module Sim = Cyclesim.With_interface (I) (T.O)

  let bits_of_bools a =
    Array.foldi a ~init:0 ~f:(fun i acc b -> if b then acc lor (1 lsl i) else acc)
  ;;

  (** Drive the stimulus for sample [n] from [wave]. *)
  let drive (stim : Bits.t ref T.Model_stimulus.t) (wave : Wave.t) n ~bubbles =
    let now = Wave.sample_time wave n in
    let stages a = Int.min (now - a) T.length in
    let set r w v = r := Bits.of_int_trunc ~width:w v in
    let fw = T.fine_width in
    set stim.edge 1 0;
    set stim.glitch 1 0;
    set stim.stages fw 0;
    set stim.glitch_stages fw 0;
    (match Wave.edges_in_cycle wave n with
     | [] -> ()
     | [ a ] ->
       set stim.edge 1 1;
       set stim.stages fw (stages a)
     | [ a1; a2 ] ->
       set stim.edge 1 1;
       set stim.stages fw (stages a1);
       set stim.glitch 1 1;
       set stim.glitch_stages fw (stages a2)
     | _ -> failwith "more than two edges in a cycle");
    (* The level after the first edge of the cycle. *)
    let after_first =
      match Wave.edges_in_cycle wave n with
      | a :: _ -> Wave.level wave a
      | [] -> false
    in
    set stim.rising 1 (Bool.to_int after_first);
    set stim.bubbles T.positions bubbles
  ;;

  (** Run the full TDC in Model mode for [samples] samples, with [clear] high
      at the samples [clear_at] selects. Returns, for every sample [n], the
      event decoded from it (read two cycles later). *)
  let run ?(clear_at = fun n -> n < n0) ?(bubbles = fun _ -> 0) (wave : Wave.t) ~samples =
    let scope = Scope.create ~flatten_design:true () in
    let sim =
      Sim.create (fun (i : _ I.t) ->
        T.create
          (Model { stages_per_cycle = wave.stages_per_cycle; stimulus = i.stim })
          scope
          i.tdc)
    in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs ~clock_edge:After sim in
    let read () : Event.t =
      let b r = Bits.to_bool !r
      and n r = Bits.to_int_trunc !r in
      { valid = b o.valid
      ; rising = b o.rising
      ; fine = n o.fine
      ; coarse = n o.coarse
      ; overflow = b o.overflow
      ; multi_edge = b o.multi_edge
      }
    in
    let results = Array.create ~len:samples Event.none in
    for c = 0 to samples + 1 do
      if c < samples
      then (
        drive i.stim wave c ~bubbles:(bubbles c);
        i.tdc.clear := Bits.of_bool (clear_at c);
        i.tdc.hit := Bits.of_bool (Wave.level wave (Wave.sample_time wave c)))
      else drive i.stim wave c ~bubbles:0;
      Cyclesim.cycle sim;
      if c >= 2 then results.(c - 2) <- read ()
    done;
    results
  ;;

  (** The analog part alone: the sampled vector after each cycle. *)
  module Analog_I = struct
    type 'a t =
      { clock : 'a
      ; stim : 'a T.Model_stimulus.t [@rtlprefix "stim_"]
      }
    [@@deriving hardcaml]
  end

  module Analog_O = struct
    type 'a t = { sampled : 'a [@bits T.positions] } [@@deriving hardcaml]
  end

  module Analog_sim = Cyclesim.With_interface (Analog_I) (Analog_O)

  let run_analog ?(bubbles = fun _ -> 0) (wave : Wave.t) ~samples =
    let sim =
      Analog_sim.create (fun (i : _ Analog_I.t) ->
        { Analog_O.sampled =
            T.Analog.create
              ~name:"tdc"
              (Model { stages_per_cycle = wave.stages_per_cycle; stimulus = i.stim })
              ~clock:i.clock
              ~hit:Signal.gnd
        })
    in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs ~clock_edge:After sim in
    Array.init samples ~f:(fun n ->
      drive i.stim wave n ~bubbles:(bubbles n);
      Cyclesim.cycle sim;
      Bits.to_int_trunc !(o.sampled))
  ;;
end

let make ~length ~coarse_width ~window =
  (module Tdc.Make (struct
      let length = length
      let coarse_width = coarse_width
      let multi_edge_window = window
    end) : Tdc.S)
;;

(** Check every sample against the physics and the reference decoder.
    Returns the number of failures and the events seen, by kind. *)
let check_against_oracles
  (module T : Tdc.S)
  ?(clear_at = fun n -> n < n0)
  (wave : Wave.t)
  ~samples
  =
  let module H = Harness (T) in
  let got = H.run ~clear_at wave ~samples in
  let failures = ref 0 in
  let events = ref [] in
  (* coarse = clock edges since clear was last released *)
  let coarse = Array.create ~len:samples 0 in
  for n = 1 to samples - 1 do
    coarse.(n)
    <- (if clear_at (n - 1) then 0 else coarse.(n - 1) + 1) % (1 lsl T.coarse_width)
  done;
  Array.iteri got ~f:(fun n got ->
    let physical =
      if clear_at n
      then Event.none
      else
        Wave.expected wave ~length:T.length ~window:T.multi_edge_window ~coarse:coarse.(n) n
    in
    let reference =
      if clear_at n || n = 0
      then Event.none
      else
        reference_decode
          ~length:T.length
          ~window:T.multi_edge_window
          ~level:(Wave.level wave (Wave.sample_time wave (n - 1)))
          ~coarse:coarse.(n)
          (Wave.sampled wave ~length:T.length n)
    in
    if not (Event.is_none got) then events := got :: !events;
    if not (Event.equal got physical && Event.equal got reference)
    then (
      Int.incr failures;
      if !failures <= 5
      then
        print_s
          [%message
            "mismatch"
              (n : int)
              ~edges:(Wave.edges_in_cycle wave n : int list)
              (got : Event.t)
              (physical : Event.t)
              (reference : Event.t)]));
  !failures, !events
;;

let summary (events : Event.t list) =
  Printf.sprintf
    "%4d valid (%3d overflow), %3d multi_edge"
    (List.count events ~f:(fun e -> e.valid))
    (List.count events ~f:(fun e -> e.overflow))
    (List.count events ~f:(fun e -> e.multi_edge))
;;

let b2s b = if b then "1" else "0"

let%expect_test "exhaustive: every arrival position, both polarities" =
  (* Length 8, 16 stages per cycle: the line is shorter than a cycle. *)
  let ((module T) as tdc) = make ~length:8 ~coarse_width:8 ~window:9 in
  let s = 16 in
  let module H = Harness (T) in
  (* All 16 arrival positions a cycle allows; k >= 8 passes the whole line.
     The edge is sampled at sample 6, i.e. coarse 6 - n0 = 4. *)
  printf " k | valid rising fine overflow coarse (rising) | (falling)\n";
  let total_failures = ref 0 in
  for k = 0 to s - 1 do
    let row =
      List.map [ true; false ] ~f:(fun rising ->
        (* An edge in the cycle ending at sample 6. For a falling edge, rise
           first, early enough for the line to settle. *)
        let edges =
          if rising then [| (6 * s) - k |] else [| (3 * s) - 5; (6 * s) - k |]
        in
        let wave = { Wave.stages_per_cycle = s; edges } in
        (* Every sample, including the edge-free ones, against both oracles. *)
        let failures, events = check_against_oracles tdc wave ~samples:10 in
        total_failures := !total_failures + failures;
        if List.length events <> Array.length edges
        then printf "k=%d: %d events, expected %d\n" k (List.length events) (Array.length edges);
        let e = (H.run wave ~samples:10).(6) in
        Printf.sprintf
          "%s %s %d %s %d"
          (b2s e.valid)
          (b2s e.rising)
          e.fine
          (b2s e.overflow)
          e.coarse)
    in
    printf "%2d | %s | %s\n" k (List.nth_exn row 0) (List.nth_exn row 1)
  done;
  printf "failures: %d\n" !total_failures;
  [%expect {|
     k | valid rising fine overflow coarse (rising) | (falling)
     0 | 1 1 0 0 4 | 1 0 0 0 4
     1 | 1 1 1 0 4 | 1 0 1 0 4
     2 | 1 1 2 0 4 | 1 0 2 0 4
     3 | 1 1 3 0 4 | 1 0 3 0 4
     4 | 1 1 4 0 4 | 1 0 4 0 4
     5 | 1 1 5 0 4 | 1 0 5 0 4
     6 | 1 1 6 0 4 | 1 0 6 0 4
     7 | 1 1 7 0 4 | 1 0 7 0 4
     8 | 1 1 8 1 4 | 1 0 8 1 4
     9 | 1 1 8 1 4 | 1 0 8 1 4
    10 | 1 1 8 1 4 | 1 0 8 1 4
    11 | 1 1 8 1 4 | 1 0 8 1 4
    12 | 1 1 8 1 4 | 1 0 8 1 4
    13 | 1 1 8 1 4 | 1 0 8 1 4
    14 | 1 1 8 1 4 | 1 0 8 1 4
    15 | 1 1 8 1 4 | 1 0 8 1 4
    failures: 0
    |}]
;;

let%expect_test "model: sampled vector matches the physics" =
  (* Random waveforms with up to two edges per cycle, random bubbles, lines
     shorter and longer than a cycle. *)
  let rng = Random.State.make [| 1 |] in
  List.iter
    [ 8, 16; 8, 9; 8, 8; 8, 5; 16, 7; 3, 2; 1, 1 ]
    ~f:(fun (length, s) ->
      let (module T) = make ~length ~coarse_width:4 ~window:1 in
      let module H = Harness (T) in
      let samples = 3000 in
      let wave =
        Wave.random
          rng
          ~stages_per_cycle:s
          ~min_gap:1
          ~max_gap:(2 * s)
          ~per_cycle:2
          ~start:1
          ~stop:((samples - 1) * s)
      in
      let bubbles _ = Random.State.int rng (1 lsl T.positions) in
      let bubble_list = Array.init samples ~f:bubbles in
      let got = H.run_analog wave ~samples ~bubbles:(fun n -> bubble_list.(n)) in
      let bad =
        Array.counti got ~f:(fun n g ->
          g <> H.bits_of_bools (Wave.sampled wave ~length n) lxor bubble_list.(n))
      in
      printf
        "length %2d, %2d stages/cycle: %d edges, %d mismatches\n"
        length
        s
        (Array.length wave.edges)
        bad);
  [%expect {|
    length  8, 16 stages/cycle: 2954 edges, 0 mismatches
    length  8,  9 stages/cycle: 2785 edges, 0 mismatches
    length  8,  8 stages/cycle: 2828 edges, 0 mismatches
    length  8,  5 stages/cycle: 2675 edges, 0 mismatches
    length 16,  7 stages/cycle: 2744 edges, 0 mismatches
    length  3,  2 stages/cycle: 2405 edges, 0 mismatches
    length  1,  1 stages/cycle: 2018 edges, 0 mismatches
    |}]
;;

let%expect_test "random waveforms against both oracles" =
  (* Edges at least 2 stages apart, up to two per cycle; lines shorter and
     longer than a cycle (then the line holds earlier cycles' edges). *)
  let rng = Random.State.make [| 2 |] in
  List.iter
    [ (* length, stages per cycle, window *)
      8, 16, 9 (* line shorter than a cycle: overflow possible *)
    ; 8, 9, 9 (* just covers a cycle *)
    ; 8, 5, 5 (* longer: the line holds the previous cycle *)
    ; 16, 7, 7
    ; 16, 7, 4 (* window narrower than a cycle *)
    ; 12, 3, 3
    ]
    ~f:(fun (length, s, window) ->
      let tdc = make ~length ~coarse_width:6 ~window in
      let samples = 3000 in
      let wave =
        Wave.random
          rng
          ~stages_per_cycle:s
          ~min_gap:2
          ~max_gap:(3 * s)
          ~per_cycle:2
          ~start:((n0 * s) + 1)
          ~stop:((samples - 3) * s)
      in
      let failures, events = check_against_oracles tdc wave ~samples in
      printf
        "length %2d, %2d stages/cycle, window %2d: %4d edges -> %s; %d failures\n"
        length
        s
        window
        (Array.length wave.edges)
        (summary events)
        failures);
  [%expect {|
    length  8, 16 stages/cycle, window  9: 1893 edges -> 1411 valid (701 overflow), 168 multi_edge; 0 failures
    length  8,  9 stages/cycle, window  9: 1871 edges -> 1431 valid (157 overflow), 220 multi_edge; 0 failures
    length  8,  5 stages/cycle, window  5: 1743 edges -> 1489 valid (  0 overflow), 127 multi_edge; 0 failures
    length 16,  7 stages/cycle, window  7: 1796 edges -> 1426 valid (  0 overflow), 185 multi_edge; 0 failures
    length 16,  7 stages/cycle, window  4: 1814 edges -> 1440 valid (  0 overflow), 149 multi_edge; 0 failures
    length 12,  3 stages/cycle, window  3: 1622 edges -> 1476 valid (  0 overflow),  73 multi_edge; 0 failures
    |}]
;;

let%expect_test "bubbles: at most one per sample, at any position >= 1" =
  (* Half the samples get a bubble; on samples with an edge, half of those are
     next to the transition (positions k-1..k+2). *)
  let rng = Random.State.make [| 3 |] in
  List.iter
    [ 8, 16, 9; 8, 9, 9; 12, 6, 6; 16, 9, 9 ]
    ~f:(fun (length, s, window) ->
      let (module T) = make ~length ~coarse_width:6 ~window in
      let module H = Harness (T) in
      let samples = 4000 in
      (* Edges at least 3 stages apart: a bubble can shorten a run by one, and
         the filter needs runs of 2. *)
      let wave =
        Wave.random
          rng
          ~stages_per_cycle:s
          ~min_gap:3
          ~max_gap:(3 * s)
          ~per_cycle:1
          ~start:((n0 * s) + 1)
          ~stop:((samples - 3) * s)
      in
      (* [k] clipped to the line: the transition is between k and k+1. *)
      let arrival n =
        match Wave.edges_in_cycle wave n with
        | [ a ] -> Some (Int.min (Wave.sample_time wave n - a) length)
        | _ -> None
      in
      let bubble =
        Array.init samples ~f:(fun n ->
          if Random.State.bool rng
          then None
          else (
            match arrival n with
            | Some k when Random.State.bool rng ->
              List.filter [ k - 1; k; k + 1; k + 2 ] ~f:(fun j -> j >= 1 && j <= length)
              |> List.random_element ~random_state:rng
            | _ -> Some (1 + Random.State.int rng length)))
      in
      let mask n = Option.value_map bubble.(n) ~default:0 ~f:(fun j -> 1 lsl j) in
      let got = H.run wave ~samples ~bubbles:mask in
      let failures = ref 0 in
      let fine_error = Map.empty (module Int) |> ref in
      let near = ref 0 in
      for n = n0 to samples - 1 do
        let coarse = (n - n0) % 64 in
        let sampled = Wave.sampled wave ~length n in
        Option.iter bubble.(n) ~f:(fun j -> sampled.(j) <- not sampled.(j));
        let reference =
          reference_decode
            ~length
            ~window
            ~level:(Wave.level wave (Wave.sample_time wave (n - 1)))
            ~coarse
            sampled
        in
        let physical = Wave.expected wave ~length ~window ~coarse n in
        let g = got.(n) in
        let ok =
          Event.equal g reference
          &&
          match arrival n, bubble.(n) with
          | Some k, Some j when j >= k - 1 && j <= k + 2 ->
            Int.incr near;
            fine_error
            := Map.update !fine_error (g.fine - k) ~f:(fun c ->
                 1 + Option.value c ~default:0);
            (* next to the transition: within one bin *)
            Int.abs (g.fine - physical.fine) <= 1
            && Event.equal { g with fine = physical.fine; overflow = physical.overflow } physical
          | _ ->
            (* no bubble, or one at least 2 positions from the transition, or
               no edge: exact *)
            Event.equal g physical
        in
        if not ok
        then (
          Int.incr failures;
          if !failures <= 5
          then
            print_s
              [%message
                "mismatch"
                  (n : int)
                  ~arrival:(arrival n : int option)
                  ~bubble:(bubble.(n) : int option)
                  (g : Event.t)
                  (physical : Event.t)
                  (reference : Event.t)])
      done;
      printf
        "length %2d, %2d stages/cycle: %d bubbles, %d next to an edge (fine error: %s); %d failures\n"
        length
        s
        (Array.count bubble ~f:Option.is_some)
        !near
        (Map.to_alist !fine_error
         |> List.map ~f:(fun (e, c) -> Printf.sprintf "%+d x%d" e c)
         |> String.concat ~sep:", ")
        !failures);
  [%expect {|
    length  8, 16 stages/cycle: 1959 bubbles, 812 next to an edge (fine error: -1 x645, +1 x167); 0 failures
    length  8,  9 stages/cycle: 2016 bubbles, 831 next to an edge (fine error: -1 x457, +1 x374); 0 failures
    length 12,  6 stages/cycle: 2024 bubbles, 737 next to an edge (fine error: -1 x307, +1 x430); 0 failures
    length 16,  9 stages/cycle: 1997 bubbles, 719 next to an edge (fine error: -1 x327, +1 x392); 0 failures
    |}]
;;

let%expect_test "coarse counter: the sampling edge's index; clear restarts it" =
  (* Clear is high for samples 0-1 and 20-21: coarse restarts at 0 on
     samples 2 and 22, and the edge sampled at 21 is not reported. *)
  let ((module T) as tdc) = make ~length:8 ~coarse_width:3 ~window:9 in
  let module H = Harness (T) in
  let s = 16 in
  (* Edges in the cycles ending at these samples, 5 stages before the clock. *)
  let at = [ 3; 5; 9; 12; 17; 21; 22; 23; 31 ] in
  let wave =
    { Wave.stages_per_cycle = s; edges = Array.of_list (List.map at ~f:(fun n -> (n * s) - 5)) }
  in
  let clear_at n = n < 2 || (n >= 20 && n < 22) in
  let got = H.run ~clear_at wave ~samples:34 in
  Array.iteri got ~f:(fun n e ->
    if not (Event.is_none e)
    then printf "sample %2d: coarse %d fine %d\n" n e.coarse e.fine);
  let failures, _ = check_against_oracles tdc ~clear_at wave ~samples:34 in
  printf "failures: %d\n" failures;
  [%expect {|
    sample  3: coarse 1 fine 5
    sample  5: coarse 3 fine 5
    sample  9: coarse 7 fine 5
    sample 12: coarse 2 fine 5
    sample 17: coarse 7 fine 5
    sample 22: coarse 0 fine 5
    sample 23: coarse 1 fine 5
    sample 31: coarse 1 fine 5
    failures: 0
    |}]
;;

let%expect_test "two edges in one cycle: flagged, not decoded" =
  let show ~length ~s ~window cases =
    let ((module T) as tdc) = make ~length ~coarse_width:8 ~window in
    let module H = Harness (T) in
    printf "length %d, %d stages/cycle, window %d\n" length s window;
    List.iter cases ~f:(fun (what, edges) ->
      let wave = { Wave.stages_per_cycle = s; edges = Array.of_list edges } in
      let got = H.run wave ~samples:12 in
      let failures, _ = check_against_oracles tdc wave ~samples:12 in
      let events =
        Array.to_list got
        |> List.filter_mapi ~f:(fun n e ->
          if Event.is_none e
          then None
          else
            Some
              (Printf.sprintf
                 "@%d valid %s rising %s fine %d multi %s"
                 n
                 (b2s e.valid)
                 (b2s e.rising)
                 e.fine
                 (b2s e.multi_edge)))
      in
      printf
        "  %-38s %s%s\n"
        what
        (String.concat ~sep:"; " events)
        (if failures = 0 then "" else Printf.sprintf " (%d FAILURES)" failures))
  in
  (* Edge times: sample n is at n * s; "n:k" below means k stages before it. *)
  let t s n k = (n * s) - k in
  let s = 16 in
  show
    ~length:8
    ~s
    ~window:9
    [ "pulse 6:6..6:2", [ t s 6 6; t s 6 2 ]
    ; "pulse 6:6..6:2, then edge 8:4", [ t s 6 6; t s 6 2; t s 8 4 ]
    ; "rise 3:3, low pulse 6:7..6:1", [ t s 3 3; t s 6 7; t s 6 1 ]
    ; "pulse 6:12..6:5, partly past the line", [ t s 6 12; t s 6 5 ]
    ; "pulse 6:3..6:2, 1 stage (filtered)", [ t s 6 3; t s 6 2 ]
    ; "pulse 6:14..6:10, past the line (lost)", [ t s 6 14; t s 6 10 ]
    ];
  (* A line longer than a cycle: it still holds the previous cycle's edge. *)
  let s = 5 in
  show
    ~length:8
    ~s
    ~window:5
    [ "pulse 6:4..6:1", [ t s 6 4; t s 6 1 ]
    ; "edge 5:0, pulse 6:4..6:2", [ t s 5 0; t s 6 4; t s 6 2 ]
    ];
  show ~length:8 ~s ~window:3 [ "pulse 6:4..6:2, outside window (lost)", [ t s 6 4; t s 6 2 ] ];
  [%expect {|
    length 8, 16 stages/cycle, window 9
      pulse 6:6..6:2                         @6 valid 0 rising 0 fine 2 multi 1
      pulse 6:6..6:2, then edge 8:4          @6 valid 0 rising 0 fine 2 multi 1; @8 valid 1 rising 1 fine 4 multi 0
      rise 3:3, low pulse 6:7..6:1           @3 valid 1 rising 1 fine 3 multi 0; @6 valid 0 rising 1 fine 1 multi 1
      pulse 6:12..6:5, partly past the line  @6 valid 0 rising 0 fine 5 multi 1
      pulse 6:3..6:2, 1 stage (filtered)
      pulse 6:14..6:10, past the line (lost)
    length 8, 5 stages/cycle, window 5
      pulse 6:4..6:1                         @6 valid 0 rising 0 fine 1 multi 1
      edge 5:0, pulse 6:4..6:2               @5 valid 1 rising 1 fine 0 multi 0; @6 valid 0 rising 1 fine 2 multi 1
    length 8, 5 stages/cycle, window 3
      pulse 6:4..6:2, outside window (lost)
    |}]
;;

let%expect_test "misuse is rejected" =
  let try_ f =
    match f () with
    | _ -> print_endline "accepted"
    | exception e -> print_s (Exn.sexp_of_t e)
  in
  try_ (fun () -> make ~length:0 ~coarse_width:8 ~window:1);
  try_ (fun () -> make ~length:8 ~coarse_width:0 ~window:9);
  try_ (fun () -> make ~length:8 ~coarse_width:8 ~window:0);
  try_ (fun () -> make ~length:8 ~coarse_width:8 ~window:10);
  let (module T) = make ~length:4 ~coarse_width:8 ~window:5 in
  let clock = Signal.input "clock" 1 in
  let hit = Signal.input "hit" 1 in
  let stimulus = T.Model_stimulus.Of_signal.inputs () in
  let cells cell = T.Cells (force lib, cell) in
  try_ (fun () -> T.Analog.create ~name:"x" (cells "sg13cmos5l_nand2_1") ~clock ~hit);
  try_ (fun () -> T.Analog.create ~name:"x" (cells "sg13cmos5l_dfrbpq_1") ~clock ~hit);
  try_ (fun () ->
    T.Analog.create ~name:"x" (cells "sg13cmos5l_buf_1") ~clock ~hit:(Signal.input "h2" 2));
  try_ (fun () ->
    T.Analog.create ~name:"x" (Model { stages_per_cycle = 0; stimulus }) ~clock ~hit);
  try_ (fun () ->
    T.Analog.create
      ~name:"x"
      (Model
         { stages_per_cycle = 4; stimulus = { stimulus with bubbles = Signal.input "b" 4 } })
      ~clock
      ~hit);
  (* Model's stimulus comes from outside the module, so it can't be a
     separate Verilog module. *)
  try_ (fun () ->
    T.hierarchical
      ~module_name:"protoemu_tdc"
      (Model { stages_per_cycle = 4; stimulus })
      (Scope.create ~flatten_design:false ())
      { clock; clear = Signal.input "clear" 1; hit });
  [%expect {|
    ("Tdc: length must be at least 1" (length 0))
    ("Tdc: coarse_width must be at least 1" (coarse_width 0))
    ("Tdc: multi_edge_window must be in [1, length + 1]" (multi_edge_window 0)
     (length 8))
    ("Tdc: multi_edge_window must be in [1, length + 1]" (multi_edge_window 10)
     (length 8))
    ("not a buffer/delay cell" (cell sg13cmos5l_nand2_1))
    ("not a buffer/delay cell" (cell sg13cmos5l_dfrbpq_1))
    ("Tdc: bad configuration" (msg "hit must be 1 bit"))
    ("Tdc: bad configuration" (msg "stages_per_cycle must be at least 1"))
    ("Port width mismatch in interface" (port_name bubbles) (expected_width 5)
     (actual_width 4))
    ("Tdc.Model is simulation-only: use it in a scope with ~flatten_design:true"
     (module_name protoemu_tdc))
    |}]
;;

let%expect_test "cells: hand-placed delay cells and kept, stably named sampling registers" =
  let (module T) = make ~length:3 ~coarse_width:4 ~window:4 in
  let clock = Signal.input "clock" 1 in
  let hit = Signal.input "hit" 1 in
  let sampled = T.Analog.create ~name:"tdc" (Cells (force lib, "sg13cmos5l_buf_1")) ~clock ~hit in
  Rtl.print Verilog (Circuit.create_exn ~name:"tdc_analog" [ Signal.output "sampled" sampled ]);
  [%expect {|
    module tdc_analog (
        clock,
        hit,
        sampled
    );

        input clock;
        input hit;
        output [3:0] sampled;

        (* keep=1,dont_touch=1 *)
        reg tdc_sample0;
        (* keep=1,dont_touch=1 *)
        reg tdc_sample1;
        (* keep=1,dont_touch=1 *)
        reg tdc_sample2;
        wire signal_inst;
        (* stable_name=1 *)
        wire tdc_stage0__X;
        wire signal_inst_1;
        (* stable_name=1 *)
        wire tdc_stage1__X;
        wire signal_inst_2;
        (* stable_name=1 *)
        wire tdc_stage2__X;
        (* keep=1,dont_touch=1 *)
        reg tdc_sample3;
        wire [3:0] signal_cat;
        always @(posedge clock) begin
            tdc_sample0 <= hit;
        end
        always @(posedge clock) begin
            tdc_sample1 <= tdc_stage0__X;
        end
        always @(posedge clock) begin
            tdc_sample2 <= tdc_stage1__X;
        end
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_buf_1
            tdc_stage0
            ( .A(hit),
              .X(signal_inst) );
        assign tdc_stage0__X = signal_inst;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_buf_1
            tdc_stage1
            ( .A(tdc_stage0__X),
              .X(signal_inst_1) );
        assign tdc_stage1__X = signal_inst_1;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_buf_1
            tdc_stage2
            ( .A(tdc_stage1__X),
              .X(signal_inst_2) );
        assign tdc_stage2__X = signal_inst_2;
        always @(posedge clock) begin
            tdc_sample3 <= tdc_stage2__X;
        end
        assign signal_cat = { tdc_sample3,
                              tdc_sample2,
                              tdc_sample1,
                              tdc_sample0 };
        assign sampled = signal_cat;

    endmodule
    |}]
;;

let%expect_test "cells: whole TDC as a separate module named by the caller; passes circuit checks" =
  let (module T) = make ~length:3 ~coarse_width:4 ~window:4 in
  let module C = Circuit.With_interface (T.I) (T.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit =
    C.create_exn
      ~name:"top"
      (T.hierarchical ~module_name:"protoemu_tdc" (Cells (force lib, "sg13cmos5l_buf_1")) scope)
  in
  let verilog =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
    |> Rtl.full_hierarchy
    |> Rope.to_string
  in
  (* The full decoder is long; show the module structure, the cells and the
     sampling registers. *)
  String.split_lines verilog
  |> List.filter ~f:(fun l ->
    List.exists [ "module "; "sg13cmos5l_"; "keep"; "tdc_s"; "protoemu_tdc" ] ~f:(fun p ->
      String.is_substring l ~substring:p))
  |> List.iter ~f:print_endline;
  [%expect {|
    module protoemu_tdc (
        (* keep=1,dont_touch=1 *)
        reg tdc_sample0;
        (* keep=1,dont_touch=1 *)
        reg tdc_sample1;
        (* keep=1,dont_touch=1 *)
        reg tdc_sample2;
        wire tdc_stage0__X;
        wire tdc_stage1__X;
        wire tdc_stage2__X;
        (* keep=1,dont_touch=1 *)
        reg tdc_sample3;
            tdc_sample0 <= signal_wire_3;
            tdc_sample1 <= tdc_stage0__X;
            tdc_sample2 <= tdc_stage1__X;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_buf_1
            tdc_stage0
        assign tdc_stage0__X = signal_inst;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_buf_1
            tdc_stage1
            ( .A(tdc_stage0__X),
        assign tdc_stage1__X = signal_inst_1;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_buf_1
            tdc_stage2
            ( .A(tdc_stage1__X),
        assign tdc_stage2__X = signal_inst_2;
            tdc_sample3 <= tdc_stage2__X;
        assign signal_cat_7 = { tdc_sample3,
                                tdc_sample2,
                                tdc_sample1,
                                tdc_sample0 };
    module top (
        protoemu_tdc
            protoemu_tdc
    |}]
;;
