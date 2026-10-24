(** Liberty boolean functions, e.g. ["!(A1&A2 | B1)"] or ["(A*B)+C'"].

    Precedence, loosest first, as in OpenSTA: [+ |] (or), [* &] (and), [^]
    (xor), juxtaposition (and), then the prefix [!] and postfix ['] negations. *)

open! Base

type t =
  | Const of bool
  | Var of string
  | Not of t
  | And of t * t
  | Or of t * t
  | Xor of t * t
[@@deriving sexp_of, equal]

exception Parse_error of string

let parse (s : string) : t =
  let n = String.length s in
  let i = ref 0 in
  let rec skip () = if !i < n && Char.is_whitespace s.[!i] then (Int.incr i; skip ()) in
  let peek () = skip (); if !i < n then Some s.[!i] else None in
  let err msg = raise (Parse_error (Printf.sprintf "%s at %d in %S" msg !i s)) in
  (* Precedence, loosest first, as in OpenSTA's liberty/LibExprParse.yy:
     [+ |], then [* &], then [^], then juxtaposition (implicit and of
     operands), then [!] and postfix ['] on an operand. *)
  let rec or_ () =
    let l = and_ () in
    match peek () with
    | Some ('+' | '|') -> Int.incr i; Or (l, or_ ())
    | _ -> l
  and and_ () =
    let l = xor_ () in
    match peek () with
    | Some ('*' | '&') -> Int.incr i; And (l, and_ ())
    | _ -> l
  and xor_ () =
    let l = juxtaposed () in
    match peek () with
    | Some '^' -> Int.incr i; Xor (l, xor_ ())
    | _ -> l
  and juxtaposed () =
    let l = unary () in
    match peek () with
    | Some c when Char.is_alphanum c || Char.equal c '(' || Char.equal c '!' || Char.equal c '_' ->
      And (l, juxtaposed ())
    | _ -> l
  and unary () =
    let base =
      match peek () with
      | Some '!' -> Int.incr i; Not (unary ())
      | Some '(' ->
        Int.incr i;
        let e = or_ () in
        if not (Option.equal Char.equal (peek ()) (Some ')')) then err "expected ')'";
        Int.incr i;
        e
      | Some c when Char.is_alphanum c || Char.equal c '_' ->
        let start = !i in
        while !i < n && (Char.is_alphanum s.[!i] || Char.equal s.[!i] '_') do Int.incr i done;
        (match String.sub s ~pos:start ~len:(!i - start) with
         | "0" -> Const false
         | "1" -> Const true
         | v -> Var v)
      | _ -> err "expected an operand"
    in
    let rec postfix e = match peek () with Some '\'' -> Int.incr i; postfix (Not e) | _ -> e in
    postfix base
  in
  let e = or_ () in
  if Option.is_some (peek ()) then err "trailing input";
  e
;;

let rec vars = function
  | Const _ -> []
  | Var v -> [ v ]
  | Not a -> vars a
  | And (a, b) | Or (a, b) | Xor (a, b) -> vars a @ vars b
;;

let vars e = List.dedup_and_sort (vars e) ~compare:String.compare

let rec eval e ~env =
  match e with
  | Const b -> b
  | Var v -> env v
  | Not a -> not (eval a ~env)
  | And (a, b) -> eval a ~env && eval b ~env
  | Or (a, b) -> eval a ~env || eval b ~env
  | Xor (a, b) -> Bool.( <> ) (eval a ~env) (eval b ~env)
;;

(** Build the function as Hardcaml logic, mapping variables with [var]. *)
let rec to_signal e ~var =
  let open Hardcaml.Signal in
  match e with
  | Const b -> if b then vdd else gnd
  | Var v -> var v
  | Not a -> ~:(to_signal a ~var)
  | And (a, b) -> to_signal a ~var &: to_signal b ~var
  | Or (a, b) -> to_signal a ~var |: to_signal b ~var
  | Xor (a, b) -> to_signal a ~var ^: to_signal b ~var
;;
