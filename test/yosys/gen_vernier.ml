(* gen_vernier LIBERTY PART: print the Verilog of the chip's Vernier (2 pins,
   40 coarse taps of dlygate4sd3/inv/dlygate4sd3/inv, 16 fine taps of
   buf_1) as module [protoemu_vernier] under a wrapper (PART = vernier), or
   flattened into [vernier_top] (PART = flat, for sta_vernier.sh), or of one
   fraction -> tap mapper alone (PART = map).

   gen_vernier LIBERTY sdc PERIOD_NS SETTLE_CYCLES: print the signoff SDC
   fragment (Sdc.vernier) for the flattened design. *)

open! Base
open Hardcaml
open Hardcaml_clockless

module V = Vernier.Make (struct
    let pins = 2
    let coarse_taps = 40
    let fine_taps = 16
    let fraction_bits = 8
  end)

let cells lib : Vernier.cells =
  { library = lib
  ; coarse_stage =
      [ "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1"; "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1" ]
  ; fine_stage = [ "sg13cmos5l_buf_1" ]
  ; inverter = "sg13cmos5l_inv_1"
  ; mux2 = "sg13cmos5l_mux2_1"
  ; xor2 = "sg13cmos5l_xor2_1"
  ; buffer = "sg13cmos5l_buf_1"
  ; flop = { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
  }
;;

let flat lib =
  let module C = Circuit.With_interface (V.I) (V.O) in
  C.create_exn ~name:"vernier_top" (V.create (Cells (cells lib)) (Scope.create ~flatten_design:true ()))
;;

let () =
  match Sys.get_argv () with
  | [| _; liberty; "sdc"; period_ns; settle_cycles |] ->
    let sdc =
      Sdc.create (flat (Cell_library.load liberty))
      |> Sdc.vernier
           ~name:"vernier"
           ~pins:V.pins
           ~period_ns:(Float.of_string period_ns)
           ~settle_cycles:(Int.of_string settle_cycles)
    in
    Stdio.print_string (Sdc.sdc sdc ~stage:Signoff ~system_clock:"clk")
  | [| _; liberty; "flat" |] -> Rtl.print Verilog (flat (Cell_library.load liberty))
  | [| _; liberty; part |] ->
    let lib = Cell_library.load liberty in
    let scope = Scope.create ~flatten_design:false () in
    let circuit =
      match part with
      | "vernier" ->
        let module C = Circuit.With_interface (V.I) (V.O) in
        C.create_exn
          ~name:"vernier_top"
          (V.hierarchical ~module_name:"protoemu_vernier" (Cells (cells lib)) scope)
      | "map" ->
        let module C = Circuit.With_interface (V.Map.I) (V.Map.O) in
        C.create_exn ~name:"vernier_top" V.Map.create
      | _ -> failwith "PART: vernier, flat or map"
    in
    Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
  | _ ->
    Stdio.eprintf "usage: gen_vernier LIBERTY (vernier|flat|map)\n       gen_vernier LIBERTY sdc PERIOD_NS SETTLE_CYCLES\n";
    Stdlib.exit 2
;;
