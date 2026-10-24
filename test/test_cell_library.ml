open! Base
open Stdio
open Hardcaml_clockless

let lib = lazy (Cell_library.load "data/sg13cmos5l_excerpt.lib")

let%expect_test "cells, kinds and pins" =
  let lib = force lib in
  print_endline lib.name;
  List.iter lib.cells ~f:(fun c ->
    printf
      "%-26s %-13s %7.2f  in:%s  out:%s\n"
      c.name
      (Sexp.to_string (Cell_library.Kind.sexp_of_t c.kind))
      c.area
      (String.concat ~sep:"," (List.map (Cell_library.Cell.inputs c) ~f:(fun p -> p.name)))
      (String.concat ~sep:"," (List.map (Cell_library.Cell.outputs c) ~f:(fun p -> p.name))));
  [%expect {|
    sg13cmos5l_stdcell_typ_1p20V_25C
    sg13cmos5l_inv_1           Combinational    5.44  in:A  out:Y
    sg13cmos5l_buf_1           Combinational    7.26  in:A  out:X
    sg13cmos5l_nand2_1         Combinational    7.26  in:A,B  out:Y
    sg13cmos5l_nor2_1          Combinational    7.26  in:A,B  out:Y
    sg13cmos5l_o21ai_1         Combinational    9.07  in:A1,A2,B1  out:Y
    sg13cmos5l_a21oi_1         Combinational    9.07  in:A1,A2,B1  out:Y
    sg13cmos5l_xor2_1          Combinational   14.52  in:A,B  out:X
    sg13cmos5l_mux2_1          Combinational   18.14  in:A0,A1,S  out:X
    sg13cmos5l_dlygate4sd1_1   Combinational   14.52  in:A  out:X
    sg13cmos5l_dlygate4sd3_1   Combinational   16.33  in:A  out:X
    sg13cmos5l_dlhq_1          Latch           30.84  in:D,GATE  out:Q
    sg13cmos5l_dfrbpq_1        Flop            48.99  in:CLK,D,RESET_B  out:Q
    sg13cmos5l_ebufn_2         Tristate        18.14  in:A,TE_B  out:Z
    sg13cmos5l_tiehi           Constant         7.26  in:  out:L_HI
    sg13cmos5l_fill_1          Physical         1.81  in:  out:
    |}]
;;

(* Independent reference semantics, written from the cell names rather than
   read from the liberty file, so the parser is checked against an oracle. *)
let reference : (string * ((string -> bool) -> bool)) list =
  [ "sg13cmos5l_inv_1", (fun v -> not (v "A"))
  ; "sg13cmos5l_buf_1", (fun v -> v "A")
  ; "sg13cmos5l_nand2_1", (fun v -> not (v "A" && v "B"))
  ; "sg13cmos5l_nor2_1", (fun v -> not (v "A" || v "B"))
  ; "sg13cmos5l_o21ai_1", (fun v -> not ((v "A1" || v "A2") && v "B1"))
  ; "sg13cmos5l_a21oi_1", (fun v -> not ((v "A1" && v "A2") || v "B1"))
  ; "sg13cmos5l_xor2_1", (fun v -> Bool.( <> ) (v "A") (v "B"))
  ; "sg13cmos5l_mux2_1", (fun v -> if v "S" then v "A1" else v "A0")
  ; "sg13cmos5l_dlygate4sd1_1", (fun v -> v "A")
  ; "sg13cmos5l_dlygate4sd3_1", (fun v -> v "A")
  ]
;;

let%expect_test "liberty functions match reference semantics exhaustively" =
  let lib = force lib in
  List.iter reference ~f:(fun (name, f) ->
    let cell = Cell_library.find_exn lib name in
    let inputs = List.map (Cell_library.Cell.inputs cell) ~f:(fun p -> p.name) in
    let out = List.hd_exn (Cell_library.Cell.outputs cell) in
    let expr = Option.value_exn out.function_ in
    let n = List.length inputs in
    let mismatches =
      List.count (List.init (1 lsl n) ~f:Fn.id) ~f:(fun bits ->
        let env pin =
          match List.findi inputs ~f:(fun _ p -> String.equal p pin) with
          | Some (i, _) -> (bits lsr i) land 1 = 1
          | None -> raise_s [%message "unknown pin" (pin : string)]
        in
        Bool.( <> ) (Bool_expr.eval expr ~env) (f env))
    in
    printf "%-26s %2d rows, %d mismatches, buffer=%b\n" name (1 lsl n) mismatches
      (Cell_library.Cell.is_buffer cell));
  [%expect {|
    sg13cmos5l_inv_1            2 rows, 0 mismatches, buffer=false
    sg13cmos5l_buf_1            2 rows, 0 mismatches, buffer=true
    sg13cmos5l_nand2_1          4 rows, 0 mismatches, buffer=false
    sg13cmos5l_nor2_1           4 rows, 0 mismatches, buffer=false
    sg13cmos5l_o21ai_1          8 rows, 0 mismatches, buffer=false
    sg13cmos5l_a21oi_1          8 rows, 0 mismatches, buffer=false
    sg13cmos5l_xor2_1           4 rows, 0 mismatches, buffer=false
    sg13cmos5l_mux2_1           8 rows, 0 mismatches, buffer=false
    sg13cmos5l_dlygate4sd1_1    2 rows, 0 mismatches, buffer=true
    sg13cmos5l_dlygate4sd3_1    2 rows, 0 mismatches, buffer=true
    |}]
;;
