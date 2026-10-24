(* gen_island LIBERTY verilog|sdc|check NETLIST: a flattened stub island (a
   17-stage ring with flop-cell counters and a cell-built system-clock sampler,
   a 4-pin Pin_capture on that ring, a 16-stage TDC) as module [island_top].

   - [verilog] prints its Verilog;
   - [sdc] prints the LibreLane dont-touch regex and the PnR / signoff SDC
     fragments;
   - [check NETLIST] checks a gate-level netlist of it (yosys or OpenROAD
     output) with [Sdc.check] and exits non-zero on any violation. *)

open! Base
open Hardcaml
open Hardcaml_clockless

module R = Ring.Make (struct
    let lengths = [ 17 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 9
  end)

module Cap =
  Pin_capture.Make
    (struct
      let pins = 4
      let slots = 2
      let guard_cycles = 4
      let order_tolerance = 4
      let status_width = 8
      let model_captures = 4
      let clock_leaf_fanout = 12
    end)
    (R)

module T = Tdc.Make (struct
    let length = 16
    let coarse_width = 8
    let multi_edge_window = 8
  end)

let flop : R.flop =
  { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
;;

let island lib =
  let open Signal in
  let clock = input "clk" 1 in
  let clear = input "clear" 1 in
  let ring_enable = input "ring_enable" 1 in
  let pins = input "pins" 4 in
  let hit = input "hit" 1 in
  let length_sel = zero R.select_width in
  let state, loop_breaks =
    R.Analog.create
      ~name:"ring"
      (Cells
         { library = lib
         ; nand2 = "sg13cmos5l_nand2_1"
         ; inverter = "sg13cmos5l_inv_1"
         ; mux2 = "sg13cmos5l_mux2_1"
         ; flop = Some flop
         })
      ~enable:ring_enable
      ~length_sel
  in
  let scope = Scope.create ~flatten_design:true () in
  let sampled =
    R.Clk_sampler.create
      ~first_stage:(lib, flop)
      ~name:"ring"
      scope
      { clock; clear; enable = ring_enable; length_sel; state }
  in
  let cap =
    Cap.create
      ~first_stage:(lib, flop)
      (Cells
         { library = lib
         ; nand2 = "sg13cmos5l_nand2_1"
         ; xor2 = "sg13cmos5l_xor2_1"
         ; inverter = "sg13cmos5l_inv_1"
         ; buffer = "sg13cmos5l_buf_1"
         ; mux2 = "sg13cmos5l_mux2_1"
         ; flop
         ; delay = "sg13cmos5l_dlygate4sd3_1", 2
         })
      scope
      { clock
      ; clear
      ; clear_status = gnd
      ; pin_enable = ones 4
      ; length_sel
      ; raw_select = zero Cap.I.port_widths.raw_select
      ; pins
      ; ring = state
      }
  in
  let tdc = T.create (Cells (lib, "sg13cmos5l_buf_1")) scope { clock; clear; hit } in
  let outputs =
    R.Clk_sampler.O.(to_list (map2 port_names sampled ~f:(fun n s -> output ("ring_" ^ n) s)))
    @ Cap.O.(to_list (map2 port_names cap ~f:(fun n s -> output ("cap_" ^ n) s)))
    @ T.O.(to_list (map2 port_names tdc ~f:(fun n s -> output ("tdc_" ^ n) s)))
  in
  Circuit.create_exn ~name:"island_top" outputs, loop_breaks
;;

let constraints lib =
  let circuit, loop_breaks = island lib in
  let loop_breaks =
    List.map loop_breaks ~f:(fun (b : R.loop_break) ->
      { Sdc.Endpoint.instance = b.instance; pin = b.pin })
  in
  circuit, Sdc.create circuit
  |> Sdc.ring ~name:"ring" ~loop_breaks ~period_ns:1.0
  |> Sdc.tdc ~name:"tdc"
  |> Sdc.pin_capture ~name:"pin_capture" ~pins:4 ~min_edge_spacing_ns:2.0
;;

let () =
  match Sys.get_argv () with
  | [| _; liberty; "verilog" |] ->
    let circuit, _ = island (Cell_library.load liberty) in
    Rtl.print Verilog circuit
  | [| _; liberty; "sdc" |] ->
    let _, sdc = constraints (Cell_library.load liberty) in
    Stdio.printf "RSZ_DONT_TOUCH_RX: %s\n\n" (Sdc.rsz_dont_touch_rx sdc);
    Stdio.print_string (Sdc.sdc sdc ~stage:Pnr ~system_clock:"clk");
    Stdio.print_string (Sdc.sdc sdc ~stage:Signoff ~system_clock:"clk")
  | [| _; liberty; "check"; netlist |] ->
    let library = Cell_library.load liberty in
    let _, sdc = constraints library in
    let nl = Netlist.load netlist in
    (match Sdc.check sdc ~library nl with
     | [] ->
       Stdio.printf
         "%d protected instances and %d protected nets found intact\n"
         (List.length (Sdc.instances sdc))
         (List.length (Sdc.protected_nets sdc));
       Stdio.printf
         "RSZ_DONT_TOUCH_LIST resolved from the netlist: %d names\n"
         (List.length (Sdc.dont_touch_list sdc ~library nl))
     | errors ->
       List.iter errors ~f:Stdio.print_endline;
       Stdio.printf "%d violations\n" (List.length errors);
       Stdlib.exit 1)
  | _ ->
    Stdio.eprintf "usage: gen_island LIBERTY verilog|sdc|check NETLIST\n";
    Stdlib.exit 2
;;
