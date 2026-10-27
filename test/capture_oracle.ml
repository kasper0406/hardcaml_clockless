(* Independent check of Pin_capture's reports against the true pin edges.

   Every true edge must be either
   - reported by a timed event attributed to its pin, with the right level,
     whose time estimate is within [match_tol] of the edge (validated), or
   - reported by an untimed (level-check) event, or as stale (level, no
     time), or
   - covered by a flag that can stand for it (below).
   Every attribution must match a true edge. A fault or overflow discards
   everything unread: an edge captured two or more cycles before the pulse
   cannot be reported by a timed event that leaves after the events in
   flight ([in_flight] cycles). A timed attribution without a
   time estimate, or whose estimate matches no edge, is not trusted: it is
   judged like a phantom. Flagged events are not trusted at all: their
   attributions count as flagged, consume no edge, and the edges they might
   have reported must be covered by a flag. Anything else is an unflagged
   error.

   Coverage, with [c] the error's reference cycle (the first cycle at or
   after a lost edge; the output cycle of a phantom):
   - a per-pin flag (lost edge, level mismatch, merge error) or a reconfig
     report on that pin, from [c] (a phantom: [c - back]) up to
     [deadline c]. The level and
     count checks run only when the FIFO is empty and the pin was stable for
     a cycle, so no constant bounds their latency under continuous activity.
     The deadline is set by the stimulus instead: the end of the first quiet
     stretch (no edge on any pin for [quiet_cycles]) starting at or after
     [c] ({!deadlines}). A flag after it is a late flag, an error;
   - a fault or overflow pulse at [f], which discards what is unread and
     resynchronises until the resync interval that follows ends at [r]:
     errors with [c] in [[f - before, r + after]], on any pin;
   - a runt pulse at [f] (bank order re-aligned, no resync): [c] in
     [[f - before, f + after]], on any pin;
   - a flagged event (fault, overflow, order or pattern error) output at
     [o]: lost edges on the pins it attributes captured in [[o - event_age,
     o]].
   Known blind spot (design): the edge counters are 2 bits, so four edges
   lost on one pin with no level change are invisible to the level and
   count checks.

   Overflow contract (owner decision D-13): an overflow discards everything
   unread and resynchronises; the next usable event is marked as following
   a loss, the pin levels are re-established by an untimed report, and no
   exact count of lost captures is claimed from the overflow alone (the
   status counter counts overflow marks, not captures). The coverage above
   implements the first and last parts. For the other two the oracle counts,
   per resync, whether the first event after it is marked although no new
   fault or overflow is pending, and whether an untimed event re-baselined
   the levels in between ({!pp_resyncs}). Pin_capture currently does
   neither: it marks the events in flight when the overflow is detected
   (those read before the gap), and takes the synchronised levels silently
   when the resync ends. *)

open! Base

type edge =
  { time : int
  ; pin : int
  ; level : bool (* after the edge *)
  }

type event =
  { cycle : int (* output cycle *)
  ; untimed : bool
  ; pins : int
  ; changed : int
  ; reconfig : int
  ; stale : int (* level reported without a time *)
  ; flagged : bool (* fault, overflow, order or pattern error *)
  ; loss_mark : bool (* fault or overflow mark (the D-13 loss tag) *)
  ; est : int option (* absolute capture time estimate, timed events *)
  }

(* The drain's flags, by kind. *)
type flags =
  { pin : (int * int) list (* (cycle, pin mask): lost edge, level mismatch, merge error *)
  ; faults : int list (* fault or overflow pulse: starts a resync *)
  ; resync : int list (* cycles with [resyncing] high *)
  ; runts : int list
  }

(* Coverage windows, in cycles (see the top). *)
type windows =
  { in_flight : int (* cycles a read event may still leave after a fault *)
  ; back : int
  ; before : int
  ; after : int
  ; event_age : int
  }

type summary =
  { edges : int
  ; timed : int (* reported by a timed event, attribution and time validated *)
  ; untimed : int (* reported by a level-check event, or stale *)
  ; flagged_lost : int (* not reported, covered by a flag *)
  ; flagged_bad : int (* attribution in a flagged event, or a bad one covered by a flag *)
  ; unflagged_lost : int
  ; unflagged_phantom : int (* attribution with no matching edge, or without a time *)
  ; late : int (* errors above whose only flag came after the deadline *)
  ; no_estimate : int (* timed attributions without a time estimate *)
  ; pending : int (* too close to the end to judge *)
  ; latencies : int list (* per-pin flag latency (cycles after [c]) of each error it covers *)
  ; resyncs : int (* resync episodes *)
  ; resync_next_marked : int (* ... whose first event after is marked, with no new fault pending *)
  ; resync_rebaselined : int (* ... with an untimed event from the resync to that event *)
  ; matches : (event * edge * int) list (* timed event, its edge, edges in the event *)
  ; errors : string list (* the unflagged errors *)
  }
[@@deriving fields]

let pp s =
  Printf.sprintf
    "edges %d: timed %d, untimed %d, lost+flagged %d, bad+flagged %d | UNFLAGGED: lost %d, phantom %d (late flag %d) | pending %d"
    s.edges s.timed s.untimed s.flagged_lost s.flagged_bad s.unflagged_lost s.unflagged_phantom
    s.late s.pending
;;

let pp_resyncs s =
  Printf.sprintf
    "resyncs %d: first event after it loss-marked %d, levels re-baselined by an untimed event %d"
    s.resyncs s.resync_next_marked s.resync_rebaselined
;;

let ok s = s.unflagged_lost = 0 && s.unflagged_phantom = 0 && s.pending = 0

(* [deadlines ~quiet_cycles ~end_cycle edge_cycles]: for each cycle [c], the
   end of the first stretch of [quiet_cycles] cycles without an edge that
   starts at or after [c]; [end_cycle] if there is none. *)
let deadlines ~quiet_cycles ~end_cycle edge_cycles =
  let marks = List.sort edge_cycles ~compare:Int.compare |> Array.of_list in
  fun c ->
    (* the first edge at or after c, and the gaps between edges after it *)
    let rec from k start =
      if k >= Array.length marks
      then if start + quiet_cycles <= end_cycle then start + quiet_cycles else end_cycle
      else if marks.(k) - start >= quiet_cycles
      then start + quiet_cycles
      else from (k + 1) (Int.max start (marks.(k) + 1))
    in
    let k =
      Array.binary_search marks ~compare:Int.compare `First_greater_than_or_equal_to c
      |> Option.value ~default:(Array.length marks)
    in
    from k c
;;

(* [cycle_of_time t]: first output cycle at or after [t]. *)
let untimed_lag = 4

let check
  ~pins
  ~(edges : edge list)
  ~(events : event list)
  ~(flags : flags)
  ~cycle_of_time
  ~cycle_time
  ~match_tol
  ~(windows : windows)
  ~(deadline : int -> int)
  ~end_cycle
  =
  (* Per pin: edges in time order and the first one not yet accounted for. *)
  let queue =
    Array.init pins ~f:(fun i ->
      List.filter edges ~f:(fun e -> e.pin = i)
      |> List.sort ~compare:(fun (a : edge) b -> Int.compare a.time b.time)
      |> Array.of_list)
  in
  let head = Array.create ~len:pins 0 in
  let lost = ref [] in
  let timed = ref 0
  and untimed = ref 0
  and flagged_bad = ref 0
  and no_estimate = ref 0
  and matches = ref []
  and bad = ref [] in
  (* Per-pin flags and reconfig reports, per pin: cycles. *)
  let cover = Array.init pins ~f:(fun _ -> ref []) in
  List.iter flags.pin ~f:(fun (c, m) ->
    for i = 0 to pins - 1 do
      if m land (1 lsl i) <> 0 then cover.(i) := c :: !(cover.(i))
    done);
  List.iter events ~f:(fun ev ->
    for i = 0 to pins - 1 do
      if ev.reconfig land (1 lsl i) <> 0 then cover.(i) := ev.cycle :: !(cover.(i))
    done);
  (* Flagged events: (output cycle, pins they might stand for). *)
  let flagged_events =
    List.filter_map events ~f:(fun ev ->
      Option.some_if ev.flagged (ev.cycle, ev.changed lor ev.stale lor ev.reconfig))
  in
  List.iter events ~f:(fun ev ->
    let t_out = cycle_time ev.cycle in
    let n_changed = Int.popcount ev.changed in
    for i = 0 to pins - 1 do
      if (ev.changed lor ev.stale) land (1 lsl i) <> 0 && ev.flagged
      then
        (* A flagged event's contents are not trusted: it reports nothing,
           and the edges it might stand for must be covered by a flag. *)
        Int.incr flagged_bad
      else if (ev.changed lor ev.stale) land (1 lsl i) <> 0
      then (
        let untimed_pin = ev.untimed || ev.stale land (1 lsl i) <> 0 in
        let v = ev.pins land (1 lsl i) <> 0 in
        let q = queue.(i) in
        let discarded k =
          (* captured well before a fault or overflow pulse (whose resync
             ends no earlier than the pulse, and sees a write within two
             synchroniser cycles), and the events in flight have left by
             now: the drain discarded it unread *)
          (not untimed_pin)
          && List.exists flags.faults ~f:(fun f ->
            cycle_of_time q.(k).time <= f - 2 && f + windows.in_flight < ev.cycle)
        in
        let candidates =
          List.range head.(i) (Array.length q)
          |> List.filter ~f:(fun k ->
            q.(k).time < t_out && Bool.equal q.(k).level v && not (discarded k))
        in
        let latest_before t =
          List.filter candidates ~f:(fun k -> q.(k).time <= t) |> List.last
        in
        let pick =
          match ev.untimed, untimed_pin, ev.est with
          | true, _, _ ->
            (* The level check samples the pins about [untimed_lag] cycles
               before its report leaves. *)
            latest_before (cycle_time (ev.cycle - untimed_lag))
          | false, true, Some est ->
            (* Stale: the latest change before this capture. *)
            latest_before (est + match_tol)
          | false, true, None -> List.last candidates
          | false, false, None ->
            (* A timed attribution whose time cannot be checked. *)
            Int.incr no_estimate;
            None
          | false, false, Some est ->
            List.min_elt candidates ~compare:(fun a b ->
              Int.compare (abs (q.(a).time - est)) (abs (q.(b).time - est)))
            |> Option.filter ~f:(fun k -> abs (q.(k).time - est) <= match_tol)
        in
        match pick with
        | None ->
          (* Judged at the end, once all flags are known. *)
          bad := (ev, i, v) :: !bad
        | Some p ->
          let e = q.(p) in
          (* Edges before it were not reported. *)
          for k = head.(i) to p - 1 do
            lost := q.(k) :: !lost
          done;
          head.(i) <- p + 1;
          if untimed_pin
          then Int.incr untimed
          else (
            Int.incr timed;
            matches := (ev, e, n_changed) :: !matches))
    done);
  let end_time = cycle_time end_cycle in
  (* Unreported edges; those whose deadline is the end of the run are
     pending unless a flag already covers them. *)
  let tail = ref [] in
  Array.iteri queue ~f:(fun i q ->
    for k = head.(i) to Array.length q - 1 do
      if q.(k).time >= end_time || deadline (cycle_of_time q.(k).time) >= end_cycle
      then tail := q.(k) :: !tail
      else lost := q.(k) :: !lost
    done);
  let sorted l = List.sort l ~compare:Int.compare |> Array.of_list in
  let covers = Array.map cover ~f:(fun l -> sorted !l) in
  let first_in arr lo =
    Array.binary_search arr ~compare:Int.compare `First_greater_than_or_equal_to lo
    |> Option.map ~f:(fun k -> arr.(k))
  in
  (* Fault windows: from [before] ahead of the pulse to [after] past the end
     of the resync that follows. *)
  let resync = sorted flags.resync in
  let resync_end f =
    let rec go c = if Array.binary_search resync ~compare:Int.compare `First_equal_to (c + 1) |> Option.is_some then go (c + 1) else c in
    go f
  in
  let windows_ =
    List.map flags.faults ~f:(fun f -> f - windows.before, resync_end f + windows.after)
    @ List.map flags.runts ~f:(fun f -> f - windows.before, f + windows.after)
  in
  let in_global c = List.exists windows_ ~f:(fun (lo, hi) -> lo <= c && c <= hi) in
  let in_flagged_event pin c_capture =
    List.exists flagged_events ~f:(fun (o, mask) ->
      mask land (1 lsl pin) <> 0 && o - windows.event_age <= c_capture && c_capture <= o)
  in
  let latencies = ref [] in
  let late = ref 0 in
  (* [`Covered | `Late | `Unflagged] for an error on [pin] at [c]. *)
  let judge ~pin c ~captured =
    if in_global c || (captured && in_flagged_event pin c)
    then `Covered
    else (
      (* a lost edge cannot be flagged before it happened; a phantom's
         reference is its output cycle, which the flag may precede *)
      match first_in covers.(pin) (if captured then c else c - windows.back) with
      | Some f when f <= deadline c ->
        latencies := (f - c) :: !latencies;
        `Covered
      | Some _ -> `Late
      | None -> `Unflagged)
  in
  let pending = ref 0 in
  let flagged_tail = ref 0 in
  List.iter !tail ~f:(fun e ->
    match judge ~pin:e.pin (cycle_of_time e.time) ~captured:true with
    | `Covered -> Int.incr flagged_tail
    | `Late | `Unflagged -> Int.incr pending);
  let errors = ref [] in
  let flagged_lost = ref !flagged_tail
  and unflagged_lost = ref 0
  and phantom = ref 0 in
  List.iter (List.rev !bad) ~f:(fun (ev, i, v) ->
    let c = ev.cycle in
    match if ev.flagged then `Covered else judge ~pin:i c ~captured:false with
    | `Covered -> Int.incr flagged_bad
    | (`Late | `Unflagged) as j ->
      Int.incr phantom;
      if Poly.equal j `Late then Int.incr late;
      errors
      := Printf.sprintf "phantom%s: cycle %d pin %d level %b est %s"
           (if Poly.equal j `Late then " (late flag)" else "")
           ev.cycle i v
           (Option.value_map ev.est ~default:"-" ~f:Int.to_string)
         :: !errors);
  List.iter !lost ~f:(fun e ->
    let c = cycle_of_time e.time in
    match judge ~pin:e.pin c ~captured:true with
    | `Covered -> Int.incr flagged_lost
    | (`Late | `Unflagged) as j ->
      Int.incr unflagged_lost;
      if Poly.equal j `Late then Int.incr late;
      errors
      := Printf.sprintf "lost%s: pin %d time %d (cycle %d) level %b"
           (if Poly.equal j `Late then " (late flag)" else "")
           e.pin e.time c e.level
         :: !errors);
  (* Per resync episode (a run of [resync] cycles): does the first event
     after it carry a fault/overflow mark although no new fault or overflow
     is pending (a loss mark), and is it an untimed report of every pin's
     right level (a re-baseline)? *)
  let episodes =
    List.fold (Array.to_list resync) ~init:[] ~f:(fun acc c ->
      match acc with
      | (lo, hi) :: rest when c = hi + 1 -> (lo, c) :: rest
      | _ -> (c, c) :: acc)
    |> List.rev
  in
  (* An untimed event reports every pin's level as sampled [untimed_lag]
     cycles before it leaves: right for every pin? *)
  let levels_ok (ev : event) =
    let t = cycle_time (ev.cycle - untimed_lag) in
    Array.for_alli queue ~f:(fun i q ->
      let level =
        match Array.binary_search q ~compare:(fun (e : edge) t -> Int.compare e.time t) `Last_less_than_or_equal_to t with
        | Some k -> q.(k).level
        | None -> if Array.is_empty q then ev.pins land (1 lsl i) <> 0 else not q.(0).level
      in
      Bool.equal level (ev.pins land (1 lsl i) <> 0))
  in
  let after_resync =
    let evs = List.sort events ~compare:(fun (a : event) b -> Int.compare a.cycle b.cycle) in
    let pulses = sorted flags.faults in
    List.map episodes ~f:(fun (_, hi) ->
      match List.find evs ~f:(fun e -> e.cycle > hi) with
      | None -> false, false
      | Some next ->
        let new_pulse =
          match first_in pulses (next.cycle - 4) with
          | Some f -> f <= next.cycle + 1 (* the status pulse is registered *)
          | None -> false
        in
        ( next.loss_mark && not new_pulse
        , next.untimed && levels_ok next ))
  in
  { edges = List.length edges
  ; timed = !timed
  ; untimed = !untimed
  ; flagged_lost = !flagged_lost
  ; flagged_bad = !flagged_bad
  ; unflagged_lost = !unflagged_lost
  ; unflagged_phantom = !phantom
  ; late = !late
  ; no_estimate = !no_estimate
  ; pending = !pending
  ; latencies = !latencies
  ; resyncs = List.length episodes
  ; resync_next_marked = List.count after_resync ~f:fst
  ; resync_rebaselined = List.count after_resync ~f:snd
  ; matches = List.rev !matches
  ; errors = List.rev !errors
  }
;;
