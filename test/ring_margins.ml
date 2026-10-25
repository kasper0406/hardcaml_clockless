(* Counter-selection margins of a sampler of the ring, from recorded
   transitions (test/test_ring_timing.ml describes the method; it is shared
   by the system-clock sampler and Pin_capture's capture slots).

   Inputs are transitions as the sampler's flops see them: at their D pins,
   in one clock frame (a flop whose clock arrives [o] late has [o]
   subtracted), each with that flop's setup/hold aperture for it:
   - the wavefront: [tr.(q)] is the q-th node transition, q = 0 a rise of
     node 0, so node [q mod n] changes;
   - each counter's bit changes.

   A clock edge at [t] can read node transition [q] new if [t > T q - hold]
   and old if [t < T q + setup]. A majority-filtered position takes a value
   only if two of its three raw inputs can, so phase [q] (positions up to
   [q] new, from [q + 1] old) is possible only in [r_lo q, r_hi q], built
   from medians of three. A counter is unstable for a clock edge in [(first
   change - hold, last change + setup)] after its own node-0 edge. *)

open! Base
open Hardcaml_clockless

type transition =
  { time : int
  ; setup : int
  ; hold : int
  }

(* The sampler's aperture (setup, hold) for a transition on its D pin,
   signed, in fs: the data must be stable from edge - setup to edge + hold.
   Worst over the arcs, as Event_sim takes it. *)
let aperture (tl : Timing_library.t) ~cell ~rising ~d_slew ~clock_slew =
  let cell = Map.find_exn tl.cells cell in
  let worst prefix =
    Timing_library.Cell.constraints cell ~input:"D"
    |> List.filter_map ~f:(fun (a : Timing_library.Arc.t) ->
      if String.is_prefix a.timing_type ~prefix
      then
        Option.map
          (if rising then a.rise_constraint else a.fall_constraint)
          ~f:(fun t -> Timing_library.Table.lookup t d_slew clock_slew)
      else None)
    |> List.max_elt ~compare:Float.compare
    |> Option.value ~default:0.
  in
  Event_sim.of_ns (worst "setup_"), Event_sim.of_ns (worst "hold_")
;;

type wavefront =
  { n : int
  ; tr : transition array
  ; periods : int
  ; bins : float list (* ps, per phase, as the sampler sees them *)
  ; stage : float (* fs, mean bin of the inverter stages *)
  }

let wavefront ~n tr =
  let len = Array.length tr in
  let periods = (len - 1) / (2 * n) in
  let bin p =
    let qs = List.filter (List.range 0 (periods * 2 * n)) ~f:(fun q -> q % (2 * n) = p) in
    let seen q = tr.(q).time + tr.(q).setup in
    List.sum (module Int) qs ~f:(fun q -> seen (q + 1) - seen q)
    |> fun total -> Event_sim.to_ps total /. Float.of_int (List.length qs)
  in
  let bins = List.init (2 * n) ~f:bin in
  (* The NAND bins (ending at node 0, with the mux if any) are several
     stages wide. *)
  let stage =
    List.filteri bins ~f:(fun p _ -> p % n <> n - 1)
    |> List.sum (module Float) ~f:Fn.id
    |> fun total -> Float.of_int (Event_sim.of_ps total) /. Float.of_int ((2 * n) - 2)
  in
  { n; tr; periods; bins; stage }
;;

(* From node transitions [(node, rising, transition)] in any order: sorted,
   indexed from the first rise of node 0, checked to be a single wavefront. *)
let of_node_transitions ~n nodes =
  let nodes =
    List.sort nodes ~compare:(fun (_, _, a) (_, _, b) -> Int.compare a.time b.time)
    |> List.drop_while ~f:(fun (i, rising, _) -> not (i = 0 && rising))
    |> Array.of_list
  in
  Array.iteri nodes ~f:(fun q (i, rising, _) ->
    (* After the rise (p < n), u_i = node_i xor odd(i) becomes 1. *)
    let expect_rising = Bool.equal (q % (2 * n) < n) (i % 2 = 0) in
    if i <> q % n || Bool.( <> ) rising expect_rising
    then raise_s [%message "not a single wavefront" (q : int) (i : int)]);
  wavefront ~n (Array.map nodes ~f:(fun (_, _, t) -> t))
;;

let stages w x = Float.of_int x /. w.stage

let median3 f i =
  match List.sort [ f (i - 1); f i; f (i + 1) ] ~compare:Int.compare with
  | [ _; m; _ ] -> m
  | _ -> assert false
;;

let can_new w i = w.tr.(i).time - w.tr.(i).hold
let can_old w i = w.tr.(i).time + w.tr.(i).setup
let r_lo w q = Int.max (median3 (can_new w) q) (median3 (can_new w) (q - 1))
let r_hi w q = Int.min (median3 (can_old w) (q + 1)) (median3 (can_old w) (q + 2))

type counter =
  { safe_from : int (* L: first phase after the edge at which the new value reads whole *)
  ; safe_until : int (* E: phases into the next period before which the old value still reads *)
  ; before : float (* stages between the unstable interval and the window *)
  ; after : float (* stages between the window and the next unstable interval *)
  }

(* For a counter with edges at q = first + 2n j: per edge, the unstable
   interval; then the window [q + s, q + s + n) against it and the next.
   Edges are located by the wavefront's own times, so [changes] must be in
   the same frame. *)
let counter w ~first ~s changes =
  let n = w.n
  and tr = w.tr
  and periods = w.periods in
  let len = Array.length tr in
  let edges = Array.init periods ~f:(fun j -> first + (2 * n * j)) in
  let unstable = Array.create ~len:periods None in
  List.iter changes ~f:(fun c ->
    (* the edge this change follows *)
    match
      Array.binary_search edges `Last_less_than_or_equal_to c.time ~compare:(fun q t ->
        Int.compare tr.(q).time t)
    with
    | None -> ()
    | Some j ->
      let lo = c.time - c.hold
      and hi = c.time + c.setup in
      unstable.(j)
      <- Some
           (match unstable.(j) with
            | None -> lo, hi
            | Some (lo', hi') -> Int.min lo lo', Int.max hi hi'));
  (* Phases (relative to the edge, so negative is the period before) that can
     overlap the unstable interval: the counter is safe to read in [last + 1,
     2n + first). *)
  let first_unsafe = ref Int.max_value
  and last_unsafe = ref Int.min_value
  and before = ref Int.max_value
  and after = ref Int.max_value in
  for j = 1 to periods - 2 do
    match unstable.(j), unstable.(j + 1) with
    | Some (lo, hi), Some (lo_next, _) when edges.(j) + s + n + 4 < len ->
      for d = -n to n - 1 do
        let q = edges.(j) + d in
        if r_lo w q < hi && r_hi w q > lo
        then (
          first_unsafe := Int.min !first_unsafe d;
          last_unsafe := Int.max !last_unsafe d)
      done;
      for q = edges.(j) + s to edges.(j) + s + n - 1 do
        before := Int.min !before (r_lo w q - hi);
        after := Int.min !after (lo_next - r_hi w q)
      done
    | _ -> ()
  done;
  if !before = Int.max_value then raise_s [%message "Ring_margins: no complete period"];
  { safe_from = !last_unsafe + 1
  ; safe_until = !first_unsafe
  ; before = stages w !before
  ; after = stages w !after
  }
;;

let worst (a : counter) (b : counter) =
  List.fold [ a.before; a.after; b.before; b.after ] ~init:Float.infinity ~f:Float.min
;;

let pp_counters w ~s (a : counter) (b : counter) =
  let n = w.n in
  Printf.sprintf
    "safe phases A [%2d, %d%+d) B [%2d, %d%+d) | s=%2d margins A %4.1f/%4.1f B %4.1f/%4.1f stages"
    a.safe_from (2 * n) a.safe_until b.safe_from (2 * n) b.safe_until s a.before a.after b.before
    b.after
;;

let pp_bins w =
  let n = w.n in
  let bins = w.bins in
  let others =
    List.filteri bins ~f:(fun p _ -> not (List.mem [ 0; n - 1; n; (2 * n) - 1 ] p ~equal:Int.equal))
  in
  Printf.sprintf
    "bins: NAND %3.0f/%3.0f ps, after node 0 %2.0f/%2.0f, others %2.0f..%2.0f"
    (List.nth_exn bins (n - 1))
    (List.nth_exn bins ((2 * n) - 1))
    (List.nth_exn bins 0)
    (List.nth_exn bins n)
    (List.fold others ~init:Float.infinity ~f:Float.min)
    (List.fold others ~init:0. ~f:Float.max)
;;
