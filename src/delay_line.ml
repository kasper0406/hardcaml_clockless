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
