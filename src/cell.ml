(** Instantiate a standard cell directly, protected from synthesis (and, with
    the constraints from {!Sdc}, from P&R) optimisation. Sub-clock and
    clockless structures depend on exact gates, so they must never be left to
    logic synthesis. *)

open! Base
open Hardcaml

(** Attached to every hand-instantiated cell. [keep] stops yosys from removing
    or remapping the cell. OpenROAD and LibreLane ignore netlist attributes:
    the resizer must be told separately ([set_dont_touch], LibreLane's
    [RSZ_DONT_TOUCH_RX]), which {!Sdc} generates. Nothing here constrains
    placement or routing. *)
let attributes =
  [ Rtl_attribute.create ~value:(Bool true) "dont_touch"
  ; Rtl_attribute.create ~value:(Bool true) "keep"
  ]
;;

(* On output nets. When a net has several names, yosys keeps the one with the
   most attributes, then the one that sorts first: without this, often one of
   Hardcaml's generated names. ([keep] would keep every alias, and LibreLane's
   synthesis then joins them with buffers.) *)
let stable_name = Rtl_attribute.create ~value:(Bool true) "stable_name"

(** [instantiate lib ~cell ~instance inputs] instantiates [cell], checking the
    pin names against the library. Returns the output pins by name. Each output
    net is named [<instance>__<pin>], so that it keeps a stable name through
    synthesis (dont-touch lists and the netlist check refer to it). The double
    underscore keeps it apart from names a caller gives the same net (e.g.
    [<instance>_q]): Hardcaml's name mangler is case-insensitive. *)
let instantiate
  (lib : Cell_library.t)
  ~cell
  ~instance
  (inputs : (string * Signal.t) list)
  : string -> Signal.t
  =
  let c = Cell_library.find_exn lib cell in
  if c.dont_use then raise_s [%message "cell is marked dont_use" (cell : string)];
  let expected = List.map (Cell_library.Cell.inputs c) ~f:(fun p -> p.name) in
  let given = List.map inputs ~f:fst in
  if not (List.equal String.equal (List.sort expected ~compare:String.compare) (List.sort given ~compare:String.compare))
  then raise_s [%message "pin mismatch" (cell : string) (expected : string list) (given : string list)];
  List.iter inputs ~f:(fun (pin, s) ->
    if Signal.width s <> 1 then raise_s [%message "cell pins are 1 bit" (cell : string) (pin : string)]);
  let outputs = List.map (Cell_library.Cell.outputs c) ~f:(fun p -> p.name, 1) in
  let inst = Instantiation.create ~instance ~attributes () ~name:cell ~inputs ~outputs in
  let named =
    List.map outputs ~f:(fun (pin, _) ->
      let s = Signal.(Instantiation.output inst pin -- (instance ^ "__" ^ pin)) in
      Signal.Type.add_attribute s stable_name;
      pin, s)
  in
  fun pin ->
    match List.Assoc.find named pin ~equal:String.equal with
    | Some s -> s
    | None -> raise_s [%message "no such output pin" (cell : string) (pin : string)]
;;

(** Instantiate a single-input, single-output buffer or delay cell. *)
let buffer lib ~cell ~instance a =
  let c = Cell_library.find_exn lib cell in
  if not (Cell_library.Cell.is_buffer c)
  then raise_s [%message "not a buffer/delay cell" (cell : string)];
  let input = (List.hd_exn (Cell_library.Cell.inputs c)).name in
  let output = (List.hd_exn (Cell_library.Cell.outputs c)).name in
  instantiate lib ~cell ~instance [ input, a ] output
;;
