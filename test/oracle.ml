(** Independent NLDM arithmetic for the [Event_sim] tests.

    Reads tables straight from the Liberty syntax tree (not through
    [Timing_library]) and interpolates with its own code (the textbook
    four-weight form; outside a table, linear extrapolation from the edge
    segment as STA tools do), so a
    bug in [Timing_library]'s table handling or in [Event_sim]'s delay
    calculation shows up as a mismatch. *)

open! Base
open Hardcaml_clockless

type table =
  { i1 : float array
  ; i2 : float array
  ; v : float array array
  }

let nums s =
  String.split s ~on:','
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:Float.of_string
  |> Array.of_list
;;

let complex (g : Liberty.group) name =
  List.find_map_exn g.body ~f:(function
    | Liberty.Complex (n, vs) when String.equal n name -> Some vs
    | _ -> None)
;;

let table_of (g : Liberty.group) =
  let one name =
    match complex g name with
    | [ v ] -> nums (Liberty.value_to_string v)
    | _ -> failwith "index"
  in
  let i1 = one "index_1" in
  let v = complex g "values" |> List.map ~f:(fun r -> nums (Liberty.value_to_string r)) in
  match
    List.exists g.body ~f:(function
      | Liberty.Complex ("index_2", _) -> true
      | _ -> false)
  with
  | true -> { i1; i2 = one "index_2"; v = Array.of_list v }
  | false ->
    (* 1-D: one row along index_1 *)
    { i1; i2 = [| 0.; 1. |]; v = Array.map (List.hd_exn v) ~f:(fun x -> [| x; x |]) }
;;

let bracket a x =
  let n = Array.length a in
  let k = ref 0 in
  while !k < n - 2 && Float.( >= ) x a.(!k + 1) do
    Int.incr k
  done;
  !k
;;

let interp t x y =
  let k = bracket t.i1 x
  and l = bracket t.i2 y in
  let wx = (x -. t.i1.(k)) /. (t.i1.(k + 1) -. t.i1.(k)) in
  let wy = (y -. t.i2.(l)) /. (t.i2.(l + 1) -. t.i2.(l)) in
  ((1. -. wx) *. (1. -. wy) *. t.v.(k).(l))
  +. (wx *. (1. -. wy) *. t.v.(k + 1).(l))
  +. ((1. -. wx) *. wy *. t.v.(k).(l + 1))
  +. (wx *. wy *. t.v.(k + 1).(l + 1))
;;

type lib = Liberty.group

let load path : lib = Liberty.parse_file path

let find_group (g : Liberty.group) kind name =
  List.find_exn (Liberty.groups g kind) ~f:(fun c -> String.equal (Liberty.group_name c) name)
;;

let cell (lib : lib) name = find_group lib "cell" name
let pin_cap lib ~cell:c ~pin =
  Float.of_string (Option.value_exn (Liberty.attr (find_group (cell lib c) "pin" pin) "capacitance"))
;;

(** The timing group of [cell]'s output [out] from [related], optionally the
    one with the given [when] string. *)
let timing lib ~cell:c ~out ~related ?when_ () =
  let p = find_group (cell lib c) "pin" out in
  List.find_exn (Liberty.groups p "timing") ~f:(fun t ->
    Option.equal String.equal (Liberty.attr t "related_pin") (Some related)
    && Option.equal String.equal (Liberty.attr t "when") when_)
;;

let constraint_group lib ~cell:c ~pin ~related ~timing_type =
  let p = find_group (cell lib c) "pin" pin in
  List.find_exn (Liberty.groups p "timing") ~f:(fun t ->
    Option.equal String.equal (Liberty.attr t "related_pin") (Some related)
    && Option.equal String.equal (Liberty.attr t "timing_type") (Some timing_type))
;;

let tbl (t : Liberty.group) kind = table_of (List.hd_exn (Liberty.groups t kind))

(** Delay and output slew (ns) of an arc for an output going [rise] (or
    falling), at input slew [slew] (ns) and load [load] (pF). *)
let arc t ~rise ~slew ~load =
  if rise
  then interp (tbl t "cell_rise") slew load, interp (tbl t "rise_transition") slew load
  else interp (tbl t "cell_fall") slew load, interp (tbl t "fall_transition") slew load
;;

let fs ns = Float.iround_nearest_exn (ns *. 1e6)

(** Wire capacitance (pF) of the [wire_load] group [model] at [fanout]:
    capacitance per unit length times the length from the [fanout_length]
    points, linear between them and continued with [slope] beyond the first
    and last (not below zero), as OpenSTA estimates it. *)
let wire_cap (lib : lib) ~model ~fanout =
  let g = find_group lib "wire_load" model in
  let num name = Float.of_string (Option.value_exn (Liberty.attr g name)) in
  let pts =
    List.filter_map g.body ~f:(function
      | Liberty.Complex ("fanout_length", [ f; l ]) ->
        Some (Float.of_string (Liberty.value_to_string f), Float.of_string (Liberty.value_to_string l))
      | _ -> None)
    |> List.sort ~compare:(fun (a, _) (b, _) -> Float.compare a b)
    |> Array.of_list
  in
  let x = Float.of_int fanout in
  let last = Array.length pts - 1 in
  let fx k = fst pts.(k)
  and lx k = snd pts.(k) in
  let length =
    if Float.(x <= fx 0)
    then Float.max 0. (lx 0 -. (num "slope" *. (fx 0 -. x)))
    else if Float.(x >= fx last)
    then lx last +. (num "slope" *. (x -. fx last))
    else (
      let k = ref 0 in
      while Float.( > ) x (fx (!k + 1)) do
        Int.incr k
      done;
      let w = (x -. fx !k) /. (fx (!k + 1) -. fx !k) in
      ((1. -. w) *. lx !k) +. (w *. lx (!k + 1)))
  in
  length *. num "capacitance"
;;
