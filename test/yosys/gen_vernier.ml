(* gen_vernier LIBERTY PART: print the Verilog of the chip's Vernier (2 pins,
   40 coarse taps of dlygate4sd3/inv/dlygate4sd3/inv, 16 fine taps of
   buf_1) as module [protoemu_vernier] under a wrapper (PART = vernier), or
   of one fraction -> tap mapper alone (PART = map). *)

open! Base
open Hardcaml
open Hardcaml_clockless

module V = Vernier.Make (struct
    let pins = 2
    let coarse_taps = 40
    let fine_taps = 16
    let fraction_bits = 8
  end)

let () =
  match Sys.get_argv () with
  | [| _; liberty; part |] ->
    let lib = Cell_library.load liberty in
    let scope = Scope.create ~flatten_design:false () in
    let circuit =
      match part with
      | "vernier" ->
        let module C = Circuit.With_interface (V.I) (V.O) in
        C.create_exn
          ~name:"vernier_top"
          (V.hierarchical
             ~module_name:"protoemu_vernier"
             (Cells
                { library = lib
                ; coarse_stage =
                    [ "sg13cmos5l_dlygate4sd3_1"
                    ; "sg13cmos5l_inv_1"
                    ; "sg13cmos5l_dlygate4sd3_1"
                    ; "sg13cmos5l_inv_1"
                    ]
                ; fine_stage = [ "sg13cmos5l_buf_1" ]
                ; inverter = "sg13cmos5l_inv_1"
                ; mux2 = "sg13cmos5l_mux2_1"
                ; xor2 = "sg13cmos5l_xor2_1"
                ; flop = { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
                })
             scope)
      | "map" ->
        let module C = Circuit.With_interface (V.Map.I) (V.Map.O) in
        C.create_exn ~name:"vernier_top" V.Map.create
      | _ -> failwith "PART: vernier or map"
    in
    Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
  | _ ->
    Stdio.eprintf "usage: gen_vernier LIBERTY (vernier|map)\n";
    Stdlib.exit 2
;;
