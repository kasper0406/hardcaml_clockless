open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

let lib = lazy (Cell_library.load "data/sg13cmos5l_excerpt.lib")

module R = Ring.Make (struct
    let lengths = [ 3 ]
    let johnson_bits = 1
    let gray_width = 1
    let counter_settle_stages = 0
  end)

module T = Tdc.Make (struct
    let length = 2
    let coarse_width = 4
    let multi_edge_window = 3
  end)

(* A 3-stage ring (node 0 observed, the other nodes closed), a 2-stage TDC
   front (kept sampling registers) and a NAND with a constant input. *)
let island () =
  let library = force lib in
  let open Signal in
  let clk = input "clk" 1 in
  let en = input "en" 1 in
  let hit = input "hit" 1 in
  let x = input "x" 1 in
  let state, loop_breaks =
    R.Analog.create
      ~name:"ring"
      (Cells
         { library
         ; nand2 = "sg13cmos5l_nand2_1"
         ; inverter = "sg13cmos5l_inv_1"
         ; mux2 = "sg13cmos5l_mux2_1"
         ; flop = None
         })
      ~enable:en
      ~length_sel:gnd
  in
  let sampled =
    T.Analog.create ~name:"tdc" (Cells (library, "sg13cmos5l_buf_1")) ~clock:clk ~hit
  in
  let y =
    Cell.instantiate library ~cell:"sg13cmos5l_nand2_1" ~instance:"gate" [ "A", x; "B", vdd ] "Y"
  in
  let circuit =
    Circuit.create_exn
      ~name:"top"
      [ output "node0" state.nodes.:(0); output "sampled" sampled; output "y" y ]
  in
  let loop_breaks =
    List.map loop_breaks ~f:(fun (b : R.loop_break) ->
      { Sdc.Endpoint.instance = b.instance; pin = b.pin })
  in
  Sdc.create circuit |> Sdc.ring ~name:"ring" ~loop_breaks ~period_ns:0.5 |> Sdc.tdc ~name:"tdc"
;;

let one_buffer ~prefix =
  let b = Cell.buffer (force lib) ~cell:"sg13cmos5l_buf_1" ~instance:"b" (Signal.input "i" 1) in
  Sdc.create ~prefix (Circuit.create_exn ~name:"c" [ Signal.output "o" b ])
;;

(* What yosys + LibreLane's synthesis would write for it: the sampling
   registers became flops with generated names, the constant a tie cell. *)
let good =
  {|
// a netlist in the style of yosys write_verilog -noattr -noexpr
module top(clk, en, hit, x, node0, sampled, y);
  input clk;
  input en;
  input hit;
  input x;
  output node0;
  output [2:0] sampled;
  output y;
  wire ring_nand__Y;
  wire ring_inv1__Y;
  wire ring_inv2__Y;
  wire tdc_stage0__X;
  wire tdc_stage1__X;
  wire \tdc_sample0 ;
  wire tdc_sample1;
  wire tdc_sample2;
  wire _1_;
  wire _2_;
  sg13cmos5l_nand2_1 ring_nand (.A(en), .B(ring_inv2__Y), .Y(ring_nand__Y));
  sg13cmos5l_inv_1 ring_inv1 (.A(ring_nand__Y), .Y(ring_inv1__Y));
  sg13cmos5l_inv_1 ring_inv2 (.A(ring_inv1__Y), .Y(ring_inv2__Y));
  sg13cmos5l_buf_1 tdc_stage0 (.A(hit), .X(tdc_stage0__X));
  sg13cmos5l_buf_1 tdc_stage1 (.A(tdc_stage0__X), .X(tdc_stage1__X));
  sg13cmos5l_dfrbpq_1 _10_ (.CLK(clk), .D(hit), .RESET_B(_1_), .Q(tdc_sample0));
  sg13cmos5l_dfrbpq_1 _11_ (.CLK(clk), .D(tdc_stage0__X), .RESET_B(_1_), .Q(tdc_sample1));
  sg13cmos5l_dfrbpq_1 _12_ (.CLK(clk), .D(tdc_stage1__X), .RESET_B(_1_), .Q(tdc_sample2));
  sg13cmos5l_tiehi _13_ (.L_HI(_1_));
  sg13cmos5l_tiehi _14_ (.L_HI(_2_));
  sg13cmos5l_nand2_1 gate (.A(x), .B(_2_), .Y(y));
  assign node0 = ring_nand__Y;
  assign sampled = { tdc_sample2, tdc_sample1, \tdc_sample0  };
endmodule
|}
;;

let check netlist =
  let t = island () in
  match Sdc.check t ~library:(force lib) (Netlist.of_string netlist) with
  | [] -> print_endline "ok"
  | errors -> List.iter errors ~f:print_endline
;;

let edit ~from ~to_ =
  if not (String.is_substring good ~substring:from) then failwith ("edit: no " ^ from);
  String.substr_replace_first good ~pattern:from ~with_:to_
;;

let%expect_test "netlist parser: nets, aliases, buses, escaped names, constants" =
  let nl = Netlist.of_string good in
  let net name = Option.value_exn (Netlist.find_net nl name) in
  let names n = String.concat ~sep:" " (Netlist.net_names nl n) in
  print_endline (names (net "node0"));
  print_endline (names (net "sampled[0]"));
  let pins (i : Netlist.Instance.t) =
    List.iter i.pins ~f:(fun (pin, c) ->
      printf
        " %s:%s"
        pin
        (match c with
         | Open -> "open"
         | Net n -> names n));
    print_endline ""
  in
  pins (Option.value_exn (Netlist.find_instance nl "gate"));
  (* constants and concatenations in connections *)
  let nl =
    Netlist.of_string
      "module m(a); input [1:0] a; c u (.A({a[0], 1'b1}), .B(), .C(2'd2)); endmodule"
  in
  pins (Option.value_exn (Netlist.find_instance nl "u"));
  (* behavioural Verilog is rejected *)
  (match Netlist.of_string "module m(a); input a; always @(a) begin end endmodule" with
   | _ -> print_endline "accepted"
   | exception Netlist.Parse_error e -> print_endline e);
  [%expect
    {|
    node0 ring_nand__Y
    sampled[0] tdc_sample0
     A:x B:_2_ Y:y
     A[1]:sampled[0] tdc_sample0 A[0]:1'b1 B:open C[1]:1'b1 C[0]:1'b0
    always in module m: not a gate-level netlist
    |}]
;;

let%expect_test "constraints: dont-touch regex and SDC fragments" =
  let t = island () in
  print_endline (Sdc.rsz_dont_touch_rx t);
  print_string (Sdc.sdc t ~stage:Pnr ~system_clock:"clk");
  print_string (Sdc.sdc t ~stage:Signoff ~system_clock:"clk");
  print_endline (Sdc.rsz_dont_touch_rx (one_buffer ~prefix:"u.top."));
  [%expect
    {|
    ^(gate|gate__Y|hit|node0|ring_inv1|ring_inv1__Y|ring_inv2|ring_inv2__Y|ring_nand|ring_nand__Y|ring_node0|ring_node1|ring_node2|sampled\\?\[0\\?\]|sampled\\?\[1\\?\]|sampled\\?\[2\\?\]|tdc_sample0|tdc_sample1|tdc_sample2|tdc_stage0|tdc_stage0__X|tdc_stage1|tdc_stage1__X|y)$
    # hardcaml_clockless islands (PnR): append after the system clock is created.
    set_disable_timing [get_pins {ring_nand/B}]
    set_false_path -through [get_pins {tdc_stage0/A}]
    # Island clocks (ring) are in the signoff SDC only: CTS ignores dont_touch.
    # hardcaml_clockless islands (signoff): append after the system clock is created.
    set_disable_timing [get_pins {ring_nand/B}]
    set_false_path -through [get_pins {tdc_stage0/A}]
    create_clock -name ring -period 0.5000 [get_pins {ring_nand/Y}]
    set_clock_groups -asynchronous -group [get_clocks {clk}] -group [get_clocks {ring}]
    ^u\.top\.(b|b__X|o)$
    |}]
;;

let%expect_test "check: an intact netlist passes; dont_touch list resolved from it" =
  check good;
  print_s
    [%sexp (Sdc.dont_touch_list (island ()) ~library:(force lib) (Netlist.of_string good) : string list)];
  [%expect {|
    ok
    (_10_ _11_ _12_ gate hit node0 ring_inv1 ring_inv1__Y ring_inv2 ring_inv2__Y
     ring_nand ring_nand__Y sampled[0] sampled[1] sampled[2] tdc_sample0
     tdc_sample1 tdc_sample2 tdc_stage0 tdc_stage0__X tdc_stage1 tdc_stage1__X y)
    |}]
;;

let%expect_test "check: resized, missing or extra cells" =
  check (edit ~from:"sg13cmos5l_inv_1 ring_inv2" ~to_:"sg13cmos5l_inv_2 ring_inv2");
  check (edit ~from:"sg13cmos5l_buf_1 tdc_stage1" ~to_:"sg13cmos5l_dlygate4sd1_1 tdc_stage1");
  check (edit ~from:"sg13cmos5l_buf_1 tdc_stage1" ~to_:"sg13cmos5l_buf_1 _99_");
  check
    (edit
       ~from:"  sg13cmos5l_tiehi _13_"
       ~to_:"  sg13cmos5l_inv_1 _98_ (.A(ring_inv1__Y), .Y(_3_));\n  sg13cmos5l_tiehi _13_");
  (* an antenna diode is a passive load (by default: a Liberty cell without
     outputs; the excerpt library has none) *)
  let with_diode =
    edit
      ~from:"  sg13cmos5l_tiehi _13_"
      ~to_:"  sg13cmos5l_antennanp ANTENNA_1 (.A(ring_inv1__Y));\n  sg13cmos5l_tiehi _13_"
  in
  check with_diode;
  print_s
    [%sexp
      (Sdc.check
         (island ())
         ~passive:(String.equal "sg13cmos5l_antennanp")
         ~library:(force lib)
         (Netlist.of_string with_diode)
       : string list)];
  [%expect {|
    instance ring_inv2 is sg13cmos5l_inv_2, expected sg13cmos5l_inv_1
    instance tdc_stage1 is sg13cmos5l_dlygate4sd1_1, expected sg13cmos5l_buf_1
    missing instance tdc_stage1 (sg13cmos5l_buf_1)
    net ring_inv1__Y has an extra load _98_/A (sg13cmos5l_inv_1)
    net ring_inv1__Y has an extra load ANTENNA_1/A (sg13cmos5l_antennanp)
    ()
    |}]
;;

let%expect_test "check: buffers" =
  (* on a ring node: split *)
  check
    (edit
       ~from:"sg13cmos5l_inv_1 ring_inv2 (.A(ring_inv1__Y)"
       ~to_:"sg13cmos5l_buf_1 _97_ (.A(ring_inv1__Y), .X(_4_));\n  sg13cmos5l_inv_1 ring_inv2 (.A(_4_)");
  (* on the TDC input fork: split *)
  check
    (edit
       ~from:"sg13cmos5l_buf_1 tdc_stage0 (.A(hit)"
       ~to_:"sg13cmos5l_buf_1 _97_ (.A(hit), .X(_4_));\n  sg13cmos5l_buf_1 tdc_stage0 (.A(_4_)");
  (* on the ring enable, an ordinary input: allowed *)
  check
    (edit
       ~from:"sg13cmos5l_nand2_1 ring_nand (.A(en)"
       ~to_:"sg13cmos5l_buf_1 _97_ (.A(en), .X(_4_));\n  sg13cmos5l_nand2_1 ring_nand (.A(_4_)");
  (* ... but not from another source *)
  check (edit ~from:"ring_nand (.A(en)" ~to_:"ring_nand (.A(x)");
  [%expect {|
    net ring_inv1__Y is split: ring_inv1/Y on ring_inv1__Y; ring_inv2/A on _4_
    kept register tdc_sample0: D is not on net _4_
    net hit is named _4_ in the netlist: the dont-touch names do not match it
    ok
    net en: island loads are driven from x, not from en
    |}]
;;

let%expect_test "check: names, registers and constants" =
  (* a protected net that lost its name: the regex would not match it *)
  check (String.substr_replace_all good ~pattern:"ring_inv1__Y" ~with_:"_5_");
  (* a kept register net driven by something else than a flop *)
  check
    (edit
       ~from:"sg13cmos5l_dfrbpq_1 _11_ (.CLK(clk), .D(tdc_stage0__X), .RESET_B(_1_), .Q(tdc_sample1))"
       ~to_:"sg13cmos5l_buf_1 _11_ (.A(tdc_stage0__X), .X(tdc_sample1))");
  (* sampling the wrong tap *)
  check (edit ~from:".D(tdc_stage0__X)" ~to_:".D(tdc_stage1__X)");
  (* the constant input: a literal is fine, the wrong value is not *)
  check (edit ~from:".B(_2_)" ~to_:".B(1'b1)");
  check (edit ~from:".B(_2_)" ~to_:".B(1'b0)");
  [%expect {|
    net ring_inv1__Y is named _5_ in the netlist: the dont-touch names do not match it
    kept register tdc_sample1 is driven by _11_ (sg13cmos5l_buf_1), not a flop
    kept register tdc_sample1: D is not on net tdc_stage0__X
    ok
    gate/B should be constant 1, is on 1'b0
    |}]
;;

let%expect_test "check: prefix" =
  let t = one_buffer ~prefix:"u_island." in
  let run text =
    match Sdc.check t ~library:(force lib) (Netlist.of_string text) with
    | [] -> print_endline "ok"
    | errors -> List.iter errors ~f:print_endline
  in
  run
    {|module top(i, o); input i; output o; wire \u_island.b__X ;
      sg13cmos5l_buf_1 \u_island.b  (.A(i), .X(\u_island.b__X ));
      assign o = \u_island.b__X ; endmodule|};
  run "module top(i, o); input i; output o; sg13cmos5l_buf_1 b (.A(i), .X(o)); endmodule";
  [%expect {|
    ok
    missing instance u_island.b (sg13cmos5l_buf_1)
    |}]
;;
