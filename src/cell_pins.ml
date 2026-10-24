(** Find the pins of simple standard cells from their Liberty functions, so
    hand-placed structures work with any library: an inverter's input and
    output, a 2-input gate's inputs, a 2:1 mux's select and data inputs. Each
    function is checked exhaustively against the expected truth table. *)

open! Base

let inputs lib cell =
  List.map (Cell_library.Cell.inputs (Cell_library.find_exn lib cell)) ~f:(fun p -> p.name)
;;

let output_and_function lib cell =
  match Cell_library.Cell.outputs (Cell_library.find_exn lib cell) with
  | [ { name; function_ = Some f; _ } ] -> name, f
  | _ -> raise_s [%message "Cell_pins: expected one output with a function" (cell : string)]
;;

(* Check [cell] computes [reference] of its inputs (in library pin order). *)
let check lib cell ~arity ~what reference =
  let ins = inputs lib cell in
  let out, fn = output_and_function lib cell in
  if List.length ins <> arity
  then raise_s [%message "Cell_pins: wrong pin count" (cell : string) (what : string)];
  for bits = 0 to (1 lsl arity) - 1 do
    let env p =
      let i, _ = List.findi_exn ins ~f:(fun _ q -> String.equal p q) in
      (bits lsr i) land 1 = 1
    in
    if Bool.( <> ) (Bool_expr.eval fn ~env) (reference (List.map ins ~f:env))
    then raise_s [%message "Cell_pins: cell does not implement" (cell : string) (what : string)]
  done;
  ins, out
;;

let one lib cell ~what f =
  match check lib cell ~arity:1 ~what (function [ a ] -> f a | _ -> false) with
  | [ a ], y -> a, y
  | _ -> assert false
;;

let two lib cell ~what f =
  match check lib cell ~arity:2 ~what (function [ a; b ] -> f a b | _ -> false) with
  | [ a; b ], y -> a, b, y
  | _ -> assert false
;;

(** (input, output) *)
let inverter lib cell = one lib cell ~what:"an inverter" not

let buffer lib cell = one lib cell ~what:"a buffer" Fn.id

(** (a, b, output) *)
let nand2 lib cell = two lib cell ~what:"a nand2" (fun a b -> not (a && b))

let xor2 lib cell = two lib cell ~what:"an xor2" Bool.( <> )

(** (select, input when select = 0, input when select = 1, output) *)
let mux2 lib cell =
  let ins = inputs lib cell in
  let out, fn = output_and_function lib cell in
  let eval assign =
    Bool_expr.eval fn ~env:(fun p -> List.Assoc.find_exn assign p ~equal:String.equal)
  in
  let implements s d0 d1 =
    List.for_all (List.init 8 ~f:Fn.id) ~f:(fun b ->
      let sv = b land 1 = 1
      and v0 = b land 2 = 2
      and v1 = b land 4 = 4 in
      Bool.equal (eval [ s, sv; d0, v0; d1, v1 ]) (if sv then v1 else v0))
  in
  let candidates =
    List.concat_map ins ~f:(fun s ->
      match List.filter ins ~f:(Fn.non (String.equal s)) with
      | [ x; y ] ->
        List.filter [ s, x, y; s, y, x ] ~f:(fun (s, d0, d1) -> implements s d0 d1)
      | _ -> [])
  in
  match candidates with
  | [ (s, d0, d1) ] -> s, d0, d1, out
  | _ -> raise_s [%message "Cell_pins: cell is not a 2:1 mux" (cell : string)]
;;
