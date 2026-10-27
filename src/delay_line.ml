(** A tapped delay line: a chain of buffer/delay cells. The building block of
    time-to-digital converters (sampling the taps with a clock edge measures
    how far an input edge has travelled) and of output edge verniers. *)

open! Base
open Hardcaml

type t =
  { input : Signal.t
  ; taps : Signal.t array (** [taps.(i)] = input after [i + 1] cells *)
  ; cells : string list (** cell used for each stage *)
  }

(** [create lib ~name ~cell ~length input] builds [length] stages of [cell].
    Instance names are [<name>_stage<i>], stable so that timing constraints
    and post-layout checks can refer to them. *)
let create lib ~name ~cell ~length input =
  if length < 1 then raise_s [%message "delay line needs at least one stage" (length : int)];
  let taps = Array.create ~len:length Signal.gnd in
  let prev = ref input in
  for i = 0 to length - 1 do
    let out = Cell.buffer lib ~cell ~instance:(Printf.sprintf "%s_stage%d" name i) !prev in
    taps.(i) <- out;
    prev := out
  done;
  { input; taps; cells = List.init length ~f:(fun _ -> cell) }
;;

(** [create_stages lib ~name ~stage ~length input] builds [length] stages,
    each the chain of single-input cells [stage] (buffers, delay cells,
    inverters), tapped after each stage. Every stage must be non-inverting
    overall (an even number of inverters), so that all taps have the input's
    polarity. A stage that mixes inverting and non-inverting cells, e.g.
    [dlygate; inv; dlygate; inv], sees both transition directions in every
    stage, which cancels the cells' rise/fall delay difference along the
    line. Instance names are [<name>_stage<i>_<j>] ([j] = position in
    [stage]). *)
let create_stages lib ~name ~stage ~length input =
  if length < 1 then raise_s [%message "delay line needs at least one stage" (length : int)];
  let inverting cell =
    match Cell_pins.buffer lib cell with
    | _ -> false
    | exception _ ->
      (match Cell_pins.inverter lib cell with
       | _ -> true
       | exception _ -> raise_s [%message "not a buffer, delay cell or inverter" (cell : string)])
  in
  if List.is_empty stage then raise_s [%message "empty delay line stage"];
  if List.count stage ~f:inverting % 2 <> 0
  then raise_s [%message "delay line stage inverts" (stage : string list)];
  let one ~instance cell a =
    let i, o = if inverting cell then Cell_pins.inverter lib cell else Cell_pins.buffer lib cell in
    Cell.instantiate lib ~cell ~instance [ i, a ] o
  in
  let taps = Array.create ~len:length Signal.gnd in
  let prev = ref input in
  for i = 0 to length - 1 do
    List.iteri stage ~f:(fun j cell ->
      prev := one ~instance:(Printf.sprintf "%s_stage%d_%d" name i j) cell !prev);
    taps.(i) <- !prev
  done;
  { input; taps; cells = List.concat (List.init length ~f:(fun _ -> stage)) }
;;
