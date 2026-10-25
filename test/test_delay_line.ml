open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

let lib = lazy (Cell_library.load "data/sg13cmos5l_excerpt.lib")

let%expect_test "delay line: hand-placed, dont_touch, stable instance names" =
  let lib = force lib in
  let a = Signal.input "a" 1 in
  let dl = Delay_line.create lib ~name:"dl" ~cell:"sg13cmos5l_dlygate4sd3_1" ~length:3 a in
  let outputs = Array.to_list (Array.mapi dl.taps ~f:(fun i t -> Signal.output (Printf.sprintf "tap%d" i) t)) in
  let circuit = Circuit.create_exn ~name:"delay_line_test" outputs in
  Rtl.print Verilog circuit;
  [%expect {|
    module delay_line_test (
        a,
        tap0,
        tap1,
        tap2
    );

        input a;
        output tap0;
        output tap1;
        output tap2;

        wire signal_inst;
        (* stable_name=1 *)
        wire dl_stage2__X;
        wire signal_inst_1;
        (* stable_name=1 *)
        wire dl_stage1__X;
        wire signal_inst_2;
        (* stable_name=1 *)
        wire dl_stage0__X;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_dlygate4sd3_1
            dl_stage2
            ( .A(dl_stage1__X),
              .X(signal_inst) );
        assign dl_stage2__X = signal_inst;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_dlygate4sd3_1
            dl_stage1
            ( .A(dl_stage0__X),
              .X(signal_inst_1) );
        assign dl_stage1__X = signal_inst_1;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_dlygate4sd3_1
            dl_stage0
            ( .A(a),
              .X(signal_inst_2) );
        assign dl_stage0__X = signal_inst_2;
        assign tap0 = dl_stage0__X;
        assign tap1 = dl_stage1__X;
        assign tap2 = dl_stage2__X;

    endmodule
    |}]
;;

let%expect_test "misuse is rejected" =
  let lib = force lib in
  let a = Signal.input "a" 1 in
  let try_ f = match f () with _ -> print_endline "accepted" | exception e -> print_s (Exn.sexp_of_t e) in
  try_ (fun () -> Delay_line.create lib ~name:"x" ~cell:"sg13cmos5l_nand2_1" ~length:2 a);
  try_ (fun () -> Delay_line.create lib ~name:"x" ~cell:"sg13cmos5l_dlygate4sd3_1" ~length:0 a);
  try_ (fun () -> Cell.instantiate lib ~cell:"sg13cmos5l_nand2_1" ~instance:"n" [ "A", a ] "Y");
  try_ (fun () -> Cell.instantiate lib ~cell:"no_such_cell" ~instance:"n" [] "Y");
  [%expect {|
    ("not a buffer/delay cell" (cell sg13cmos5l_nand2_1))
    ("delay line needs at least one stage" (length 0))
    ("pin mismatch" (cell sg13cmos5l_nand2_1) (expected (A B)) (given (A)))
    ("cell not in library" (name no_such_cell)
     (library sg13cmos5l_stdcell_typ_1p20V_25C))
    |}]
;;
