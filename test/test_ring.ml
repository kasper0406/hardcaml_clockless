open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

module Config = struct
  let lengths = [ 15; 17; 21 ]
  let johnson_bits = 4
  let gray_width = 6
  let counter_settle_stages = 6
end

module R = Ring.Make (Config)

(* --- 1. Counter ---------------------------------------------------------- *)

module Counter_circuit = struct
  module I = struct
    type 'a t = { clock : 'a } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { bits : 'a [@bits R.counter_width]
      ; count : 'a [@bits R.count_width]
      }
    [@@deriving hardcaml]
  end

  let create (i : _ I.t) =
    let bits =
      Ring.Counter.create
        ~johnson_bits:Config.johnson_bits
        ~gray_width:Config.gray_width
        (Signal.Reg_spec.create ~clock:i.clock ())
    in
    { O.bits
    ; count =
        Ring.Counter.decode ~johnson_bits:Config.johnson_bits ~gray_width:Config.gray_width bits
    }
  ;;
end

let%expect_test "counter: decodes to the edge count; encoding matches the model; bit changes" =
  let module Sim = Cyclesim.With_interface (Counter_circuit.I) (Counter_circuit.O) in
  let sim = Sim.create Counter_circuit.create in
  let o = Cyclesim.outputs sim in
  let wrap = 1 lsl R.count_width in
  let errors = ref 0 in
  let encode_mismatch = ref 0 in
  let changes = Hashtbl.create (module Int) in
  let prev = ref (Bits.to_int_trunc !(o.bits)) in
  for n = 0 to (2 * wrap) + 5 do
    (* After n clock edges the counter has counted n (modulo its range). *)
    if Bits.to_int_trunc !(o.count) <> n % wrap then Int.incr errors;
    if Bits.to_int_trunc !(o.bits)
       <> Ring.Counter.encode_int ~johnson_bits:Config.johnson_bits ~gray_width:Config.gray_width n
    then Int.incr encode_mismatch;
    Cyclesim.cycle sim;
    let cur = Bits.to_int_trunc !(o.bits) in
    Hashtbl.incr changes (Int.popcount (cur lxor !prev));
    prev := cur
  done;
  printf "decode errors: %d, encoding mismatches: %d\n" !errors !encode_mismatch;
  Hashtbl.to_alist changes
  |> List.sort ~compare:[%compare: int * int]
  |> List.iter ~f:(fun (bits, n) -> printf "%d bit(s) changed: %d steps\n" bits n);
  [%expect {|
    decode errors: 0, encoding mismatches: 0
    1 bit(s) changed: 902 steps
    2 bit(s) changed: 128 steps
    |}]
;;

(* --- 2. Ideal ring vs the closed form in Ring's documentation ----------- *)

let%expect_test "ideal ring: unit-delay gate simulation matches the closed form" =
  List.iter Config.lengths ~f:(fun n ->
    let ring = Ring.Ideal.create ~n ~stages:R.max_length in
    let bad_nodes = ref 0 in
    let bad_counts = ref 0 in
    (* Let the start-up transient pass (one trip around the loop). *)
    for _ = 1 to 2 * n do Ring.Ideal.step ring done;
    for _ = 1 to 20 * n do
      Ring.Ideal.step ring;
      let t = ring.time + n - 1 in
      let p = t % (2 * n) in
      for i = 0 to R.max_length - 1 do
        (* node i (t) = node 0 (t - i) xor odd(i); node 0 is high for p in [0, n). *)
        let node0_at q = (q % (2 * n)) < n in
        let expected = Bool.( <> ) (node0_at (p - i)) (i % 2 = 1) in
        if Bool.( <> ) ring.nodes.(i) expected then Int.incr bad_nodes
      done;
      if ring.count_a <> t / (2 * n) || ring.count_b <> (t + n) / (2 * n) then Int.incr bad_counts
    done;
    printf "n=%d: node mismatches %d, count mismatches %d\n" n !bad_nodes !bad_counts);
  [%expect {|
    n=15: node mismatches 0, count mismatches 0
    n=17: node mismatches 0, count mismatches 0
    n=21: node mismatches 0, count mismatches 0
    |}]
;;

(* --- 3. End to end: Model + Clk_sampler in Cyclesim --------------------- *)

module Sampled = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; length_sel : 'a [@bits R.select_width]
      ; stim : 'a R.Model_stimulus.t [@rtlprefix "stim_"]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    let state, _ =
      R.Analog.create ~name:"ring" (Model i.stim) ~enable:i.enable ~length_sel:i.length_sel
    in
    R.Clk_sampler.create
      ~name:"ring"
      scope
      { clock = i.clock; clear = i.clear; enable = i.enable; length_sel = i.length_sel; state }
  ;;
end

let latency = 2

(* Sample an ideal ring every [stages_per_cycle] stage delays. Counters read
   within [hazard] stages of their edge return old or new at random. Returns
   (samples, errors), where an error is a decoded time that differs from the
   ideal ring's time (by more than one stage with [bubble]), or a wrong
   phase. *)
let run_sampled ?bubble ~n ~stages_per_cycle ~hazard ~cycles () =
  let has_bubble = Option.is_some bubble in
  let bubble = Option.value bubble ~default:(fun ~n:_ ~phase:_ -> None) in
  let module Sim = Cyclesim.With_interface (Sampled.I) (R.Clk_sampler.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Sampled.create scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let random = Random.State.make [| n; stages_per_cycle; hazard |] in
  let sel = List.findi_exn Config.lengths ~f:(fun _ m -> m = n) |> fst in
  i.length_sel := Bits.of_int_trunc ~width:R.select_width sel;
  i.enable := Bits.vdd;
  let ring = Ring.Ideal.create ~n ~stages:R.max_length in
  for _ = 1 to 2 * n do Ring.Ideal.step ring done;
  let history = Queue.create () in
  let errors = ref 0 in
  let samples = ref 0 in
  (* Decoded time is [k * 2n + p] with [k] modulo the counter range. *)
  let modulus = (1 lsl R.count_width) * 2 * n in
  for _ = 1 to cycles do
    for _ = 1 to stages_per_cycle do Ring.Ideal.step ring done;
    let a, b = Ring.Ideal.counts ring ~settle:hazard ~pick_old:(fun () -> Random.State.bool random) in
    let true_t = ring.time + n - 1 in
    let nodes =
      Array.foldi ring.nodes ~init:0 ~f:(fun idx acc v -> if v then acc lor (1 lsl idx) else acc)
    in
    let nodes =
      match bubble ~n ~phase:(true_t % (2 * n)) with
      | Some pos -> nodes lxor (1 lsl pos)
      | None -> nodes
    in
    i.stim.nodes := Bits.of_int_trunc ~width:R.max_length nodes;
    i.stim.count_a := Bits.of_int_trunc ~width:R.count_width a;
    i.stim.count_b := Bits.of_int_trunc ~width:R.count_width b;
    Queue.enqueue history true_t;
    Cyclesim.cycle sim;
    if Queue.length history > latency
    then (
      let t = Queue.dequeue_exn history in
      let decoded = Bits.to_int_trunc !(o.time) in
      let phase = Bits.to_int_trunc !(o.phase) in
      Int.incr samples;
      (* The decoder's time origin is the ideal ring's: decoded = true time. *)
      let d = (decoded - t) % modulus in
      let tolerance = if has_bubble then 1 else 0 in
      if Int.min d (modulus - d) > tolerance then Int.incr errors;
      if (not has_bubble) && phase <> t % (2 * n) then Int.incr errors)
  done;
  !samples, !errors
;;

let%expect_test "clk sampler recovers ring time exactly, with counters read mid-update" =
  List.iter Config.lengths ~f:(fun n ->
    List.iter [ 37; 41; 64 ] ~f:(fun stages_per_cycle ->
      let samples, errors =
        run_sampled ~n ~stages_per_cycle ~hazard:Config.counter_settle_stages ~cycles:3000 ()
      in
      printf "n=%d stages/cycle=%d: %d samples, %d errors\n" n stages_per_cycle samples errors));
  [%expect {|
    n=15 stages/cycle=37: 2998 samples, 0 errors
    n=15 stages/cycle=41: 2998 samples, 0 errors
    n=15 stages/cycle=64: 2998 samples, 0 errors
    n=17 stages/cycle=37: 2998 samples, 0 errors
    n=17 stages/cycle=41: 2998 samples, 0 errors
    n=17 stages/cycle=64: 2998 samples, 0 errors
    n=21 stages/cycle=37: 2998 samples, 0 errors
    n=21 stages/cycle=41: 2998 samples, 0 errors
    n=21 stages/cycle=64: 2998 samples, 0 errors
    |}]
;;

let%expect_test "a settle window beyond the configured one does cause errors (the margin is real)" =
  List.iter Config.lengths ~f:(fun n ->
    let thr = R.select_threshold n in
    let _, errors_ok = run_sampled ~n ~stages_per_cycle:37 ~hazard:(thr - 1) ~cycles:3000 () in
    let _, errors_bad = run_sampled ~n ~stages_per_cycle:37 ~hazard:(thr + 2) ~cycles:3000 () in
    printf "n=%d threshold=%d: hazard %d -> %d errors; hazard %d -> errors > 0: %b\n"
      n thr (thr - 1) errors_ok (thr + 2) (errors_bad > 0));
  [%expect {|
    n=15 threshold=11: hazard 10 -> 0 errors; hazard 13 -> errors > 0: true
    n=17 threshold=12: hazard 11 -> 0 errors; hazard 14 -> errors > 0: true
    n=21 threshold=14: hazard 13 -> 0 errors; hazard 16 -> errors > 0: true
    |}]
;;

let%expect_test "a single misread node next to the wavefront moves time by at most one stage" =
  (* Flip the node just behind or just ahead of the wavefront. *)
  let bubble ~n ~phase =
    let front = if phase < n then phase else phase - n in
    let pos = if front % 2 = 0 then front + 1 else front - 1 in
    if pos >= 0 && pos < n then Some pos else None
  in
  List.iter Config.lengths ~f:(fun n ->
    let samples, errors =
      run_sampled ~bubble ~n ~stages_per_cycle:41 ~hazard:Config.counter_settle_stages ~cycles:3000 ()
    in
    printf "n=%d: %d samples, %d off by more than one stage\n" n samples errors);
  [%expect {|
    n=15: 2998 samples, 0 off by more than one stage
    n=17: 2998 samples, 0 off by more than one stage
    n=21: 2998 samples, 0 off by more than one stage
    |}]
;;

(* --- 4. Cells mode ------------------------------------------------------- *)

module Small = Ring.Make (struct
    let lengths = [ 3; 5 ]
    let johnson_bits = 2
    let gray_width = 2
    let counter_settle_stages = 1
  end)

let%expect_test "cells mode: hand-placed loop, loop-break pin, counters on ring clocks" =
  let library = Cell_library.load "data/sg13cmos5l_excerpt.lib" in
  let enable = Signal.input "enable" 1 in
  let length_sel = Signal.input "length_sel" Small.select_width in
  let state, breaks =
    Small.Analog.create
      ~name:"ring"
      (Cells
         { library
         ; nand2 = "sg13cmos5l_nand2_1"
         ; inverter = "sg13cmos5l_inv_1"
         ; mux2 = "sg13cmos5l_mux2_1"
         ; flop = None
         })
      ~enable
      ~length_sel
  in
  List.iter breaks ~f:(fun b -> print_s [%sexp (b : Small.loop_break)]);
  let outputs =
    [ Signal.output "nodes" state.nodes
    ; Signal.output "count_a" state.count_a
    ; Signal.output "count_b" state.count_b
    ]
  in
  let circuit = Circuit.create_exn ~name:"ring_test" outputs in
  Rtl.print Verilog circuit;
  [%expect {|
    ((instance ring_nand) (pin B))
    module ring_test (
        length_sel,
        enable,
        nodes,
        count_a,
        count_b
    );

        input length_sel;
        input enable;
        output [4:0] nodes;
        output [3:0] count_a;
        output [3:0] count_b;

        wire signal_const;
        wire signal_select;
        reg signal_reg;
        wire signal_select_1;
        wire [1:0] signal_cat;
        wire [1:0] signal_const_3;
        wire signal_select_2;
        wire [1:0] signal_cat_1;
        wire [1:0] signal_xor;
        wire [1:0] signal_add;
        wire [1:0] signal_xor_1;
        wire [1:0] signal_const_4;
        wire signal_select_3;
        reg signal_reg_1;
        wire signal_select_4;
        wire signal_not;
        wire signal_select_5;
        wire [1:0] signal_cat_2;
        wire signal_select_6;
        reg signal_reg_2;
        wire [1:0] signal_cat_3;
        wire [1:0] signal_wire;
        wire signal_eq;
        wire [1:0] signal_mux;
        wire signal_select_7;
        reg signal_reg_3;
        wire [1:0] signal_cat_4;
        wire [1:0] signal_wire_1;
        wire [3:0] ring_count_b;
        wire signal_select_8;
        reg signal_reg_4;
        wire signal_select_9;
        wire [1:0] signal_cat_5;
        wire signal_select_10;
        wire gnd;
        wire [1:0] signal_cat_6;
        wire [1:0] signal_xor_2;
        wire [1:0] signal_add_1;
        wire [1:0] signal_xor_3;
        wire signal_select_11;
        reg signal_reg_5;
        wire signal_not_1;
        wire signal_select_12;
        wire signal_not_2;
        wire signal_select_13;
        wire [1:0] signal_cat_7;
        wire signal_select_14;
        reg signal_reg_6;
        wire [1:0] signal_cat_8;
        wire [1:0] signal_wire_2;
        wire signal_eq_1;
        wire [1:0] signal_mux_1;
        wire signal_select_15;
        reg signal_reg_7;
        wire [1:0] signal_cat_9;
        wire [1:0] signal_wire_3;
        wire [3:0] ring_count_a;
        wire signal_inst;
        wire signal_wire_4;
        wire signal_wire_5;
        wire signal_inst_1;
        wire ring_node0;
        wire signal_inst_2;
        wire ring_node1;
        wire signal_inst_3;
        wire ring_node2;
        wire signal_inst_4;
        wire ring_node3;
        wire signal_inst_5;
        wire ring_node4;
        wire [4:0] signal_cat_10;
        assign signal_const = 1'b0;
        assign signal_select = signal_mux[0:0];
        always @(negedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg <= signal_const;
            else
                signal_reg <= signal_select;
        end
        assign signal_select_1 = signal_add[1:1];
        assign signal_cat = { signal_const,
                              signal_select_1 };
        assign signal_const_3 = 2'b01;
        assign signal_select_2 = signal_wire_1[1:1];
        assign signal_cat_1 = { gnd,
                                signal_select_2 };
        assign signal_xor = signal_wire_1 ^ signal_cat_1;
        assign signal_add = signal_xor + signal_const_3;
        assign signal_xor_1 = signal_add ^ signal_cat;
        assign signal_const_4 = 2'b10;
        assign signal_select_3 = signal_cat_2[0:0];
        always @(negedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg_1 <= signal_const;
            else
                signal_reg_1 <= signal_select_3;
        end
        assign signal_select_4 = signal_wire[1:1];
        assign signal_not = ~ signal_select_4;
        assign signal_select_5 = signal_wire[0:0];
        assign signal_cat_2 = { signal_select_5,
                                signal_not };
        assign signal_select_6 = signal_cat_2[1:1];
        always @(negedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg_2 <= signal_const;
            else
                signal_reg_2 <= signal_select_6;
        end
        assign signal_cat_3 = { signal_reg_2,
                                signal_reg_1 };
        assign signal_wire = signal_cat_3;
        assign signal_eq = signal_wire == signal_const_4;
        assign signal_mux = signal_eq ? signal_xor_1 : signal_wire_1;
        assign signal_select_7 = signal_mux[1:1];
        always @(negedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg_3 <= signal_const;
            else
                signal_reg_3 <= signal_select_7;
        end
        assign signal_cat_4 = { signal_reg_3,
                                signal_reg };
        assign signal_wire_1 = signal_cat_4;
        assign ring_count_b = { signal_wire_1,
                                signal_wire };
        assign signal_select_8 = signal_mux_1[0:0];
        always @(posedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg_4 <= signal_const;
            else
                signal_reg_4 <= signal_select_8;
        end
        assign signal_select_9 = signal_add_1[1:1];
        assign signal_cat_5 = { signal_const,
                                signal_select_9 };
        assign signal_select_10 = signal_wire_3[1:1];
        assign gnd = 1'b0;
        assign signal_cat_6 = { gnd,
                                signal_select_10 };
        assign signal_xor_2 = signal_wire_3 ^ signal_cat_6;
        assign signal_add_1 = signal_xor_2 + signal_const_3;
        assign signal_xor_3 = signal_add_1 ^ signal_cat_5;
        assign signal_select_11 = signal_cat_7[0:0];
        always @(posedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg_5 <= signal_const;
            else
                signal_reg_5 <= signal_select_11;
        end
        assign signal_not_1 = ~ enable;
        assign signal_select_12 = signal_wire_2[1:1];
        assign signal_not_2 = ~ signal_select_12;
        assign signal_select_13 = signal_wire_2[0:0];
        assign signal_cat_7 = { signal_select_13,
                                signal_not_2 };
        assign signal_select_14 = signal_cat_7[1:1];
        always @(posedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg_6 <= signal_const;
            else
                signal_reg_6 <= signal_select_14;
        end
        assign signal_cat_8 = { signal_reg_6,
                                signal_reg_5 };
        assign signal_wire_2 = signal_cat_8;
        assign signal_eq_1 = signal_wire_2 == signal_const_4;
        assign signal_mux_1 = signal_eq_1 ? signal_xor_3 : signal_wire_3;
        assign signal_select_15 = signal_mux_1[1:1];
        always @(posedge ring_node0 or posedge signal_not_1) begin
            if (signal_not_1)
                signal_reg_7 <= signal_const;
            else
                signal_reg_7 <= signal_select_15;
        end
        assign signal_cat_9 = { signal_reg_7,
                                signal_reg_4 };
        assign signal_wire_3 = signal_cat_9;
        assign ring_count_a = { signal_wire_3,
                                signal_wire_2 };
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_mux2_1
            ring_mux0
            ( .S(length_sel),
              .A0(ring_node2),
              .A1(ring_node4),
              .X(signal_inst) );
        assign signal_wire_4 = signal_inst;
        assign signal_wire_5 = signal_wire_4;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_nand2_1
            ring_nand
            ( .A(enable),
              .B(signal_wire_5),
              .Y(signal_inst_1) );
        assign ring_node0 = signal_inst_1;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_inv_1
            ring_inv1
            ( .A(ring_node0),
              .Y(signal_inst_2) );
        assign ring_node1 = signal_inst_2;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_inv_1
            ring_inv2
            ( .A(ring_node1),
              .Y(signal_inst_3) );
        assign ring_node2 = signal_inst_3;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_inv_1
            ring_inv3
            ( .A(ring_node2),
              .Y(signal_inst_4) );
        assign ring_node3 = signal_inst_4;
        (* keep=1,dont_touch=1 *)
        sg13cmos5l_inv_1
            ring_inv4
            ( .A(ring_node3),
              .Y(signal_inst_5) );
        assign ring_node4 = signal_inst_5;
        assign signal_cat_10 = { ring_node4,
                                 ring_node3,
                                 ring_node2,
                                 ring_node1,
                                 ring_node0 };
        assign nodes = signal_cat_10;
        assign count_a = ring_count_a;
        assign count_b = ring_count_b;

    endmodule
    |}]
;;

(* --- 5. Configuration and naming ----------------------------------------- *)

let%expect_test "johnson_bits must be a power of two (decoded wrap = counter wrap)" =
  List.iter [ 1; 2; 3; 4; 6 ] ~f:(fun k ->
    let ok =
      try
        let module _ =
          Ring.Make (struct
            let lengths = [ 17 ]
            let johnson_bits = k
            let gray_width = 2
            let counter_settle_stages = 1
          end)
        in
        "accepted"
      with
      | _ -> "rejected"
    in
    printf "johnson_bits %d: %s\n" k ok);
  [%expect {|
    johnson_bits 1: accepted
    johnson_bits 2: accepted
    johnson_bits 3: rejected
    johnson_bits 4: accepted
    johnson_bits 6: rejected
    |}]
;;

let%expect_test "Clk_sampler first-stage flop cells keep their instance names" =
  let library = Cell_library.load "data/sg13cmos5l_excerpt.lib" in
  let dfrbpq : Small.flop =
    { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
  in
  let clock = Signal.input "clock" 1 in
  let enable = Signal.input "enable" 1 in
  let length_sel = Signal.input "length_sel" Small.select_width in
  let state, _ =
    Small.Analog.create
      ~name:"ring"
      (Cells
         { library
         ; nand2 = "sg13cmos5l_nand2_1"
         ; inverter = "sg13cmos5l_inv_1"
         ; mux2 = "sg13cmos5l_mux2_1"
         ; flop = Some dfrbpq
         })
      ~enable
      ~length_sel
  in
  let scope = Scope.create ~flatten_design:true () in
  let o =
    Small.Clk_sampler.create
      ~first_stage:(library, dfrbpq)
      ~name:"ring"
      scope
      { clock; clear = Signal.gnd; enable; length_sel; state }
  in
  let circuit =
    Circuit.create_exn
      ~name:"sampler_names"
      Small.Clk_sampler.O.(to_list (map2 port_names o ~f:Signal.output))
  in
  let verilog = Rtl.create Verilog [ circuit ] |> Rtl.full_hierarchy |> Rope.to_string in
  let expected =
    List.init Small.max_length ~f:(Printf.sprintf "ring_sample_node%d")
    @ List.init Small.counter_width ~f:(Printf.sprintf "ring_sample_count_a_%d")
    @ List.init Small.counter_width ~f:(Printf.sprintf "ring_sample_count_b_%d")
  in
  (* an instance name is on a line of its own, after the cell name *)
  let lines = List.map (String.split_lines verilog) ~f:String.strip in
  let missing =
    List.filter expected ~f:(fun nm -> not (List.mem lines nm ~equal:String.equal))
  in
  printf "first-stage instances: %d, missing or renamed: %s\n" (List.length expected)
    (if List.is_empty missing then "none" else String.concat ~sep:" " missing);
  [%expect {| first-stage instances: 13, missing or renamed: none |}]
;;
