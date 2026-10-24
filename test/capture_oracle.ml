(* Independent check of Pin_capture's reports against the true pin edges.

   Every true edge must be either
   - reported by a timed event attributed to its pin, with the right level,
     whose time is within [match_tol] of the edge (when the caller supplies
     an absolute time estimate), or
   - reported by an untimed (level-check) event, or
   - covered by a flag: a per-pin lost-edge / level-mismatch pulse on that
     pin, a reconfig report on it, an event flagged fault/overflow/order/
     pattern error, a fault/overflow pulse, or a resync interval, within a
     window around the edge.
   Every attribution must match a true edge. Flagged events are not
   trusted at all: their attributions count as flagged, consume no edge, and
   the edges they might have reported must be covered by a flag.
   Anything else is an unflagged error. *)

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
  ; est : int option (* absolute capture time estimate, timed events *)
  }

type summary =
  { edges : int
  ; timed : int (* reported by a timed event, attribution and time ok *)
  ; untimed : int (* reported by a level-check event *)
  ; flagged_lost : int (* not reported, covered by a flag *)
  ; flagged_bad : int (* wrong attribution/time in a flagged event *)
  ; unflagged_lost : int
  ; unflagged_phantom : int (* attribution with no matching edge *)
  ; pending : int (* too close to the end to judge *)
  ; matches : (event * edge * int) list (* timed event, its edge, edges in the event *)
  ; errors : string list (* the unflagged errors *)
  }
[@@deriving fields]

let pp s =
  Printf.sprintf
    "edges %d: timed %d, untimed %d, lost+flagged %d, bad+flagged %d | UNFLAGGED: lost %d, phantom %d | pending %d"
    s.edges s.timed s.untimed s.flagged_lost s.flagged_bad s.unflagged_lost s.unflagged_phantom
    s.pending
;;

(* [cycle_of_time t]: first output cycle at or after [t]. [pin_flags]:
   (cycle, mask). [global_flags]: cycles with a fault/overflow pulse or in
   resync. *)
let untimed_lag = 4

let check
  ~pins
  ~(edges : edge list)
  ~(events : event list)
  ~(pin_flags : (int * int) list)
  ~(global_flags : int list)
  ~cycle_of_time
  ~cycle_time
  ~match_tol
  ~back
  ~fwd
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
  and phantom = ref 0
  and matches = ref []
  and errors = ref []
  and bad = ref [] in
  (* Coverage marks per pin: cycles. *)
  let cover = Array.init pins ~f:(fun _ -> ref []) in
  let global = ref global_flags in
  List.iter pin_flags ~f:(fun (c, m) ->
    for i = 0 to pins - 1 do
      if m land (1 lsl i) <> 0 then cover.(i) := c :: !(cover.(i))
    done);
  List.iter events ~f:(fun ev ->
    if ev.flagged then global := ev.cycle :: !global;
    for i = 0 to pins - 1 do
      if ev.reconfig land (1 lsl i) <> 0 then cover.(i) := ev.cycle :: !(cover.(i))
    done);
  List.iter events ~f:(fun ev ->
    let t_out = cycle_time ev.cycle in
    let n_changed = Int.popcount ev.changed in
    for i = 0 to pins - 1 do
      if (ev.changed lor ev.stale) land (1 lsl i) <> 0 && ev.flagged
      then
        (* A flagged event's contents are not trusted: it reports nothing,
           and the edges it might stand for must be covered by its flag. *)
        Int.incr flagged_bad
      else if (ev.changed lor ev.stale) land (1 lsl i) <> 0
      then (
        let untimed_pin = ev.untimed || ev.stale land (1 lsl i) <> 0 in
        let v = ev.pins land (1 lsl i) <> 0 in
        let q = queue.(i) in
        let candidates =
          List.range head.(i) (Array.length q)
          |> List.filter ~f:(fun k -> q.(k).time < t_out && Bool.equal q.(k).level v)
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
          | false, _, None -> List.last candidates
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
          else if ev.flagged
          then Int.incr flagged_bad
          else (
            Int.incr timed;
            matches := (ev, e, n_changed) :: !matches))
    done);
  let end_time = cycle_time (end_cycle - fwd) in
  let pending = ref 0 in
  Array.iteri queue ~f:(fun i q ->
    for k = head.(i) to Array.length q - 1 do
      if q.(k).time < end_time then lost := q.(k) :: !lost else Int.incr pending
    done);
  let globals = List.sort !global ~compare:Int.compare |> Array.of_list in
  let covers = Array.map cover ~f:(fun l -> List.sort !l ~compare:Int.compare |> Array.of_list) in
  let any_in arr lo hi =
    match Array.binary_search arr ~compare:Int.compare `First_greater_than_or_equal_to lo with
    | Some k -> arr.(k) <= hi
    | None -> false
  in
  let flagged_lost = ref 0
  and unflagged_lost = ref 0 in
  List.iter (List.rev !bad) ~f:(fun (ev, i, v) ->
    let c = ev.cycle in
    if ev.flagged || any_in globals (c - back) (c + fwd) || any_in covers.(i) (c - back) (c + fwd)
    then Int.incr flagged_bad
    else (
      Int.incr phantom;
      errors
      := Printf.sprintf "phantom: cycle %d pin %d level %b est %s" ev.cycle i v
           (Option.value_map ev.est ~default:"-" ~f:Int.to_string)
         :: !errors));
  List.iter !lost ~f:(fun e ->
    let c = cycle_of_time e.time in
    if any_in globals (c - back) (c + fwd) || any_in covers.(e.pin) (c - back) (c + fwd)
    then Int.incr flagged_lost
    else (
      Int.incr unflagged_lost;
      errors
      := Printf.sprintf "lost: pin %d time %d (cycle %d) level %b" e.pin e.time c e.level
         :: !errors));
  { edges = List.length edges
  ; timed = !timed
  ; untimed = !untimed
  ; flagged_lost = !flagged_lost
  ; flagged_bad = !flagged_bad
  ; unflagged_lost = !unflagged_lost
  ; unflagged_phantom = !phantom
  ; pending = !pending
  ; matches = List.rev !matches
  ; errors = List.rev !errors
  }
;;
