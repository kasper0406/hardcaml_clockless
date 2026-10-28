(* gen_stub LIBERTY CMD: the stub island for the physical-flow gate (issue #7),
   as module [protoemu_island], which janestreet-bitbanger's flow/island
   wrapper instantiates as [island] next to the chip:

   - a fixed 17-stage ring with flop-cell counters and a cell-built
     system-clock sampler;
   - a 4-pin Pin_capture on that ring with cell first stages. Pins 0 and 1
     come through a loopback mux from the Vernier's pin-0 rise and fall lines
     (the D-3 calibration path) or from [gpio[1:0]]; pins 2 and 3 are
     [gpio[3:2]] directly, nets the wrapper's chip also reads (a fork shared
     with logic outside the island);
   - the chip's 2-pin Vernier (40 coarse dlygate4sd3/inv taps, 16 fine buf
     taps) on [vout[1:0]].

   Everything is controlled from a serial configuration register on two
   spare inputs: while [cfg_shift] is high, [cfg_data] shifts in at every
   clock; when it falls, the shifted bits are committed at once (vernier
   mode writes and flag clears pulse for one cycle then). Everything is
   observed: the last capture event is held, and [obs] is one bit of the
   concatenated island outputs, picked by the configuration ([obs_select]),
   and [obs_parity] their parity, both registered.

   CMD:
   - [verilog]: the Verilog;
   - [rx]: RSZ_DONT_TOUCH_RX from the circuit's names (before synthesis);
   - [sdc pnr|signoff [RING_PERIOD_NS]]: the SDC fragments;
   - [dont-touch NETLIST]: the names to protect (one per line), resolved from
     a synthesised netlist of the wrapper;
   - [rx-of LIST]: RSZ_DONT_TOUCH_RX matching exactly the names in LIST;
   - [check NETLIST [LIST [REFERENCE]]]: Sdc.check on a netlist of the
     wrapper (synthesised or routed; LIST: the resolved names, REFERENCE:
     the synthesised netlist they were resolved from), non-zero exit on any
     violation;
   - [fields]: the configuration fields, LSB first (name, offset, width).

   Instance and net names get the prefix ["island."] (the wrapper's instance,
   flattened by yosys). *)

open! Base
open Hardcaml
open Hardcaml_clockless

let prefix = "island."
let clock_period_ns = 20.

(* The smallest settle_cycles the host may load (Vernier: 3 before layout,
   4 with margin at 50 MHz). *)
let settle_cycles = 4

(* L and E for a ring that also drives Pin_capture (README). *)
module R = Ring.Make_timed (struct
    let lengths = [ 17 ]
    let johnson_bits = 4
    let gray_width = 6
    let counter_settle_stages = 9
    let counter_earliest_change_stages = 4
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

module V = Vernier.Make (struct
    let pins = 2
    let coarse_taps = 40
    let fine_taps = 16
    let fraction_bits = 8
  end)

(* The recent-edge delay must exceed the pin -> bank clock delay at every
   corner. Two dlygate4sd3 (the test benches') do before layout, not after:
   routing added 0.4-0.5 ns to the bank clocks and the delayed copy beat
   them by up to 0.55 ns (slow corner), so every capture would read stale.
   Four leave margin at all three corners after routing. *)
let delay_stages = 4

let flop : R.flop =
  { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
;;

let vernier_cells lib : Vernier.cells =
  { library = lib
  ; coarse_stage =
      [ "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1"; "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1" ]
  ; fine_stage = [ "sg13cmos5l_buf_1" ]
  ; inverter = "sg13cmos5l_inv_1"
  ; mux2 = "sg13cmos5l_mux2_1"
  ; xor2 = "sg13cmos5l_xor2_1"
  ; flop = { cell = flop.cell; clk = flop.clk; d = flop.d; reset_n = flop.reset_n; q = flop.q }
  }
;;

(* Configuration fields, LSB first. *)
let fields =
  let per n name w = List.init n ~f:(fun i -> Printf.sprintf "%s%d" name i, w) in
  [ "clear", 1
  ; "ring_enable", 1
  ; "pin_enable", Cap.I.port_widths.pin_enable
  ; "count_select", Cap.I.port_widths.count_select
  ; "clear_status", 1
  ; "loopback", 1
  ; "settle_cycles", V.I.port_widths.settle_cycles
  ]
  @ per V.pins "mode" 2
  @ per V.pins "fraction" V.fraction_bits
  @ per V.pins "level" 1
  @ per V.pins "toggle" 1 (* toggle the pin every cycle (same fraction: allowed) *)
  @ per V.lines "scale" (List.hd_exn V.I.port_widths.scale)
  @ per V.lines "offset" (List.hd_exn V.I.port_widths.offset)
  @ per V.lines "ratio" (List.hd_exn V.I.port_widths.ratio)
  @ [ "obs_select", 10 ]
;;

let config_bits = List.sum (module Int) fields ~f:snd

let island lib =
  let open Signal in
  let clock = input "clk" 1 in
  let rst_n = input "rst_n" 1 in
  let cfg_shift = input "cfg_shift" 1 in
  let cfg_data = input "cfg_data" 1 in
  let gpio = input "gpio" 4 in
  let spec = Reg_spec.create ~clock () in
  let r x = reg spec x in
  (* the configuration *)
  let sync x = r (r x) in
  let shift = sync cfg_shift in
  let data = sync cfg_data in
  let shadow = reg_fb spec ~width:config_bits ~f:(fun s -> mux2 shift (data @: msbs s) s) in
  let commit = r shift &: ~:shift in
  let active = reg spec ~enable:commit shadow -- "cfg" in
  let committed = r commit in
  let field =
    let tbl = Hashtbl.create (module String) in
    ignore
      (List.fold fields ~init:0 ~f:(fun lo (name, w) ->
         Hashtbl.add_exn tbl ~key:name ~data:(active.:[lo + w - 1, lo]);
         lo + w)
       : int);
    Hashtbl.find_exn tbl
  in
  let per n name = List.init n ~f:(fun i -> field (Printf.sprintf "%s%d" name i)) in
  let clear = r ~:rst_n |: field "clear" in
  let length_sel = zero R.select_width in
  let scope = Scope.create ~flatten_design:true () in
  (* the ring and its sampler *)
  let ring_enable = field "ring_enable" in
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
  let sampled =
    R.Clk_sampler.create
      ~first_stage:(lib, flop)
      ~name:"ring"
      scope
      { clock; clear; enable = ring_enable; length_sel; state }
  in
  (* the vernier *)
  let levels =
    List.init V.pins ~f:(fun i ->
      reg_fb spec ~width:1 ~f:(fun l ->
        mux2 (field (Printf.sprintf "toggle%d" i)) ~:l (field (Printf.sprintf "level%d" i))))
  in
  let vernier =
    V.create
      (Cells (vernier_cells lib))
      scope
      { clock
      ; clear
      ; level = levels
      ; fraction = per V.pins "fraction"
      ; mode = per V.pins "mode"
      ; mode_write = List.init V.pins ~f:(fun _ -> committed)
      ; settle_cycles = field "settle_cycles"
      ; scale = per V.lines "scale"
      ; offset = per V.lines "offset"
      ; ratio = per V.lines "ratio"
      ; clear_flags = List.init V.pins ~f:(fun _ -> committed)
      }
  in
  (* the capture, pins 0-1 through the loopback mux *)
  let line_out = List.take vernier.line_out 2 |> List.rev |> concat_msb in
  let pins = gpio.:[3, 2] @: (mux2 (field "loopback") line_out gpio.:[1, 0] -- "capture_pins") in
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
         ; delay = "sg13cmos5l_dlygate4sd3_1", delay_stages
         })
      scope
      { clock
      ; clear
      ; clear_status = field "clear_status" &: committed
      ; pin_enable = field "pin_enable"
      ; length_sel
      ; count_select = field "count_select"
      ; pins
      ; ring = state
      ; ring_time = sampled.time
      ; ring_time_valid = sampled.valid &: ~:(sampled.pattern_error)
      }
  in
  (* observation *)
  let event = Cap.Event.Of_signal.pack cap.event in
  let held = reg spec ~enable:cap.event.valid event in
  let observed =
    concat_lsb
      ([ R.Clk_sampler.O.Of_signal.pack sampled; held; Cap.Status.Of_signal.pack cap.status ]
       @ List.concat
           [ vernier.active_mode
           ; vernier.fallback
           ; vernier.overrun
           ; vernier.range_fault
           ; vernier.mismatch
           ; vernier.overrun_event
           ; vernier.range_event
           ])
  in
  if width observed > 1 lsl 10 then raise_s [%message "obs_select too narrow" (width observed : int)];
  let obs = r (mux (field "obs_select") (bits_lsb observed)) in
  let obs_parity = r (tree ~arity:4 (bits_lsb observed) ~f:(reduce ~f:( ^: ))) in
  let outputs =
    [ output "vout" (concat_msb (List.rev vernier.out))
    ; output "obs" obs
    ; output "obs_parity" obs_parity
    ]
  in
  Circuit.create_exn ~name:"protoemu_island" outputs, loop_breaks
;;

(* The ring's period for the counters' clocks: the slowest timing corner's
   paths against the typical corner's period would be pessimistic, the fast
   corner's period against typical paths fail. LibreLane's signoff (one SDC,
   violations checked at the typical corner) gets the typical period below
   (pre-layout 2.61 ns, Event_sim); flow/signoff.sh redoes each corner with
   that corner's measured period. *)
let ring_period_ns = ref 2.4

let constraints lib =
  let circuit, loop_breaks = island lib in
  let loop_breaks =
    List.map loop_breaks ~f:(fun (b : R.loop_break) ->
      { Sdc.Endpoint.instance = b.instance; pin = b.pin })
  in
  ( circuit
  , Sdc.create ~prefix circuit
    |> Sdc.ring ~name:"ring" ~loop_breaks ~period_ns:!ring_period_ns
    |> Sdc.pin_capture ~name:"pin_capture" ~pins:4 ~min_edge_spacing_ns:2.0
    |> Sdc.vernier ~name:"vernier" ~pins:V.pins ~period_ns:clock_period_ns ~settle_cycles )
;;

(* Island inputs keep the wrapper's names after flattening. *)
let boundary =
  [ "clk", "clk"; "rst_n", "rst_n" ]
  @ List.init 4 ~f:(fun i -> Printf.sprintf "gpio[%d]" i, Printf.sprintf "uio_in[%d]" i)
;;

let () =
  match Sys.get_argv () with
  | [| _; liberty; "verilog" |] ->
    let circuit, _ = island (Cell_library.load liberty) in
    Rtl.print Verilog circuit
  | [| _; liberty; "rx" |] ->
    let _, sdc = constraints (Cell_library.load liberty) in
    Stdio.print_endline (Sdc.rsz_dont_touch_rx sdc)
  | ([| _; liberty; "sdc"; stage |] | [| _; liberty; "sdc"; stage; _ |]) as argv ->
    if Array.length argv = 5 then ring_period_ns := Float.of_string argv.(4);
    let _, sdc = constraints (Cell_library.load liberty) in
    let stage : Sdc.Stage.t =
      match stage with
      | "pnr" -> Pnr
      | "signoff" -> Signoff
      | _ -> failwith "stage: pnr or signoff"
    in
    Stdio.print_string (Sdc.sdc sdc ~stage ~system_clock:"clk")
  | [| _; liberty; "dont-touch"; netlist |] ->
    let library = Timing_library.load liberty in
    let _, sdc = constraints library.library in
    List.iter
      (Or_error.ok_exn (Sdc.dont_touch_list sdc ~library (Netlist.load netlist)))
      ~f:Stdio.print_endline
  | [| _; _; "rx-of"; list |] ->
    Stdio.print_endline (Sdc.dont_touch_rx (Stdio.In_channel.read_lines list))
  | ([| _; liberty; "check"; netlist |]
    | [| _; liberty; "check"; netlist; _ |]
    | [| _; liberty; "check"; netlist; _; _ |]) as argv ->
    let library = Timing_library.load liberty in
    let _, sdc = constraints library.library in
    let dont_touch =
      if Array.length argv >= 5 then Stdio.In_channel.read_lines argv.(4) else []
    in
    let reference = if Array.length argv = 6 then Some (Netlist.load argv.(5)) else None in
    (match Sdc.check ~boundary ~dont_touch ?reference sdc ~library (Netlist.load netlist) with
     | [] ->
       (match Sdc.boundary_fanouts sdc ~library ~max_fanout:10 (Netlist.load netlist) with
        | [] -> ()
        | l ->
          Stdio.printf
            "note: %d boundary nets above the max fanout (10), not repairable: %s\n"
            (List.length l)
            (String.concat ~sep:" " (List.map l ~f:(fun (n, f) -> Printf.sprintf "%s(%d)" n f))));
       Stdio.printf
         "%s: %d protected instances and %d protected nets intact\n"
         netlist
         (List.length (Sdc.instances sdc))
         (List.length (Sdc.protected_nets sdc))
     | errors ->
       List.iter errors ~f:Stdio.print_endline;
       Stdio.printf "%s: %d violations\n" netlist (List.length errors);
       Stdlib.exit 1)
  | [| _; _; "fields" |] ->
    ignore
      (List.fold fields ~init:0 ~f:(fun lo (name, w) ->
         Stdio.printf "%s %d %d\n" name lo w;
         lo + w)
       : int)
  | _ ->
    Stdio.eprintf
      "usage: gen_stub LIBERTY verilog|rx|sdc pnr|sdc signoff|dont-touch NL|rx-of LIST|check NL [LIST [REF]]|fields\n";
    Stdlib.exit 2
;;
