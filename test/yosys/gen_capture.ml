(* gen_capture LIBERTY: print the Verilog of a Cells-mode Pin_capture (4 pins,
   2 slots per bank) with its 17-stage ring, as module [protoemu_pin_capture]
   under a wrapper. *)

open! Base
open Hardcaml
open Hardcaml_clockless

(* L and E as measured for this ring and Pin_capture in
   test/test_pin_capture_timing.ml (pre-layout, Liberty 1k wire load). *)
module R = Ring.Make_timed (struct
    let lengths = [ 17 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 9
    let counter_earliest_change_stages = 4
  end)

let () =
  match Sys.get_argv () with
  | [| _; liberty; slots |] ->
    let module Cap =
      Pin_capture.Make
        (struct
          let pins = 4
          let slots = Int.of_string slots
          let guard_cycles = 4
          let order_tolerance = 4
          let status_width = 8
          let model_captures = 4
          let clock_leaf_fanout = 12
        end)
        (R)
    in
    let lib = Cell_library.load liberty in
    let dfrbpq : R.flop =
      { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
    in
    let scope = Scope.create ~flatten_design:false () in
    let module C = Circuit.With_interface (Cap.I) (Cap.O) in
    let circuit =
      C.create_exn
        ~name:"capture_top"
        (Cap.hierarchical
           ~first_stage:(lib, dfrbpq)
           ~module_name:"protoemu_pin_capture"
           (Cells
              { library = lib
              ; nand2 = "sg13cmos5l_nand2_1"
              ; xor2 = "sg13cmos5l_xor2_1"
              ; inverter = "sg13cmos5l_inv_1"
              ; buffer = "sg13cmos5l_buf_1"
              ; mux2 = "sg13cmos5l_mux2_1"
              ; flop = dfrbpq
              ; delay = "sg13cmos5l_dlygate4sd3_1", 2
              })
           scope)
    in
    Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
  | _ ->
    Stdio.eprintf "usage: gen_capture LIBERTY SLOTS\n";
    Stdlib.exit 2
;;
