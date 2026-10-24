open! Base
open Stdio
open Hardcaml_clockless

let show s =
  match Bool_expr.parse s with
  | e -> print_s [%sexp (e : Bool_expr.t)]
  | exception Bool_expr.Parse_error m -> printf "error: %s\n" m
;;

let%expect_test "precedence and syntax variants" =
  List.iter
    [ "A+B*C" (* and binds tighter than or *)
    ; "A^B|C" (* xor binds tighter than or *)
    ; "A&B^C" (* xor binds tighter than and (OpenSTA LibExprParse.yy) *)
    ; "A^B*C"
    ; "A B^C" (* juxtaposition binds tighter than xor *)
    ; "A^B C+D"
    ; "!A^B" (* prefix not applies to the operand only *)
    ; "!A*B" (* prefix not binds tightest *)
    ; "(A1 A2)+B1" (* juxtaposition is and *)
    ; "A'" (* postfix not *)
    ; "!(A1&A2 | B1)"
    ; "1"
    ; "(A"
    ; "A+"
    ]
    ~f:show;
  [%expect {|
    (Or (Var A) (And (Var B) (Var C)))
    (Or (Xor (Var A) (Var B)) (Var C))
    (And (Var A) (Xor (Var B) (Var C)))
    (And (Xor (Var A) (Var B)) (Var C))
    (Xor (And (Var A) (Var B)) (Var C))
    (Or (Xor (Var A) (And (Var B) (Var C))) (Var D))
    (Xor (Not (Var A)) (Var B))
    (And (Not (Var A)) (Var B))
    (Or (And (Var A1) (Var A2)) (Var B1))
    (Not (Var A))
    (Not (Or (And (Var A1) (Var A2)) (Var B1)))
    (Const true)
    error: expected ')' at 2 in "(A"
    error: expected an operand at 2 in "A+"
    |}]
;;
