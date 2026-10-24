(* gen_tdc LIBERTY LENGTH CELL: print the Verilog of a Cells-mode TDC as
   module [protoemu_tdc] (hierarchy included). *)

open! Base
open Hardcaml
open Hardcaml_clockless

let () =
  match Sys.get_argv () with
  | [| _; liberty; length; cell |] ->
    let n = Int.of_string length in
    let module T =
      Tdc.Make (struct
        let length = n
        let coarse_width = 16
        let multi_edge_window = Int.min 32 (n + 1)
      end)
    in
    let module C = Circuit.With_interface (T.I) (T.O) in
    let scope = Scope.create ~flatten_design:false () in
    let lib = Cell_library.load liberty in
    let circuit =
      C.create_exn ~name:"tdc_top" (T.hierarchical ~module_name:"protoemu_tdc" (Cells (lib, cell)) scope)
    in
    Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
  | _ ->
    Stdio.eprintf "usage: gen_tdc LIBERTY LENGTH CELL\n";
    Stdlib.exit 2
;;
