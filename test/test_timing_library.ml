open! Base
open Stdio
open Hardcaml_clockless

(* Units, templates (including one declaring load as variable_1, which must be
   transposed), one-dimensional tables, multi-pin related_pin, ff groups. *)
let src =
  {|
library (synthetic) {
  time_unit : "1ps";
  capacitive_load_unit (1,ff);
  lu_table_template (load_first) {
    variable_1 : total_output_net_capacitance;
    variable_2 : input_net_transition;
    index_1 ("1, 3");
    index_2 ("10, 30");
  }
  lu_table_template (mpw) {
    variable_1 : constrained_pin_transition;
    index_1 ("10, 30");
  }
  cell (g) {
    area : 1;
    pin (A) { direction : input; capacitance : 2; }
    pin (B) { direction : input; capacitance : 4; }
    pin (Y) {
      direction : output;
      function : "A&B";
      timing () {
        related_pin : "A B";
        cell_rise (load_first) { values ("100, 200", "300, 400"); }
        cell_fall (mpw) { values ("50, 70"); }
      }
    }
  }
  cell (f) {
    pin (CK) { direction : input; clock : true; capacitance : 1; }
    pin (D) { direction : input; capacitance : 1; }
    pin (Q) { direction : output; function : "IQ"; }
    ff (IQ, IQN) { clocked_on : "CK"; next_state : "D"; }
  }
}
|}
;;

let%expect_test "timing library: units, transposed templates, 1-D tables" =
  let t = Timing_library.of_liberty (Liberty.parse_string src) in
  let g = Timing_library.find_exn t "g" in
  printf "pin caps (pF): A %g B %g\n" (Timing_library.Cell.pin_cap g "A") (Timing_library.Cell.pin_cap g "B");
  let arcs = Timing_library.Cell.arcs g ~output:"Y" in
  printf "arcs from: %s\n" (String.concat ~sep:"," (List.map arcs ~f:(fun a -> a.related_pin)));
  let a = List.hd_exn arcs in
  let rise = Option.value_exn a.cell_rise
  and fall = Option.value_exn a.cell_fall in
  (* rise table: rows = load (1, 3 fF), columns = slew (10, 30 ps), values in ps.
     After normalisation index_1 = slew (ns), index_2 = load (pF), values ns.
     By hand: 100, 200, 300 at the corners, 250 at the centre, 250 when
     extrapolating to 40 ps at 1 fF; fall 50/70 at 10/30 ps, 60 at 20, 80 at 40. *)
  List.iter [ 0.010, 0.001; 0.030, 0.001; 0.010, 0.003; 0.020, 0.002; 0.040, 0.001 ] ~f:(fun (s, l) ->
    printf "rise(slew %.3f ns, load %.3f pF) = %.4f ns; fall = %.4f ns\n" s l
      (Timing_library.Table.lookup rise s l) (Timing_library.Table.lookup fall s l));
  let f = Timing_library.find_exn t "f" in
  print_s [%sexp (f.seq : Timing_library.Seq.t option)];
  [%expect {|
    pin caps (pF): A 0.002 B 0.004
    arcs from: A,B
    rise(slew 0.010 ns, load 0.001 pF) = 0.1000 ns; fall = 0.0500 ns
    rise(slew 0.030 ns, load 0.001 pF) = 0.2000 ns; fall = 0.0700 ns
    rise(slew 0.010 ns, load 0.003 pF) = 0.3000 ns; fall = 0.0500 ns
    rise(slew 0.020 ns, load 0.002 pF) = 0.2500 ns; fall = 0.0600 ns
    rise(slew 0.040 ns, load 0.001 pF) = 0.2500 ns; fall = 0.0800 ns
    (((state IQ) (state_n IQN)
      (kind (Ff (clocked_on (Var CK)) (next_state (Var D)))) (clear ())
      (preset ()) (clear_preset_var1 ())))
    |}]
;;

(* Wire-load models and selection, minimum pulse widths (attribute over
   timing group), recovery/removal arcs kept as constraints. *)
let wl_src =
  {|
library (wl) {
  time_unit : "1ps";
  capacitive_load_unit (1,ff);
  wire_load ("small") {
    capacitance : 2;
    slope : 3;
    fanout_length (3, 5);
    fanout_length (1, 1);
  }
  wire_load ("big") {
    capacitance : 2;
    slope : 5;
    fanout_length (2, 10);
  }
  wire_load ("frac") {
    capacitance : 1;
    slope : 0;
    fanout_length (1.25, 10);
    fanout_length (2.25, 30);
  }
  wire_load_selection (sel) {
    wire_load_from_area (100, 1000, big);
    wire_load_from_area (0, 100, small);
  }
  default_wire_load : "small";
  default_wire_load_selection : sel;
  cell (f) {
    pin (CK) {
      direction : input;
      clock : true;
      capacitance : 1;
      min_pulse_width_high : 50;
      timing () {
        related_pin : "CK";
        timing_type : min_pulse_width;
        rise_constraint (scalar) { index_1 ("10, 30"); values ("100, 120"); }
        fall_constraint (scalar) { index_1 ("10, 30"); values ("70, 90"); }
      }
    }
    pin (RN) {
      direction : input;
      capacitance : 1;
      timing () {
        related_pin : "CK";
        timing_type : recovery_rising;
        rise_constraint (scalar) { index_1 ("10, 30"); index_2 ("10, 30"); values ("1, 2", "3, 4"); }
      }
    }
    pin (D) { direction : input; capacitance : 1; }
    pin (Q) { direction : output; function : "IQ"; }
    ff (IQ, IQN) { clocked_on : "CK"; next_state : "D"; clear : "!RN"; }
  }
}
|}
;;

let%expect_test "timing library: wire loads, pulse widths, recovery arcs" =
  let t = Timing_library.of_liberty (Liberty.parse_string wl_src) in
  let small = Timing_library.default_wire_load t in
  (* By hand, "small" (2 fF per unit, slope 3, points (1,1) (3,5)):
     fanout 0: 1 - 3 < 0 -> 0; 1: 1 -> 2 fF; 2: 3 -> 6 fF; 3: 5 -> 10 fF;
     5: 5 + 2*3 = 11 -> 22 fF. "big" (one point (2,10), slope 5): fanout 1:
     5 -> 10 fF; 4: 20 -> 40 fF. *)
  printf "small: %s pF\n"
    (String.concat ~sep:" "
       (List.map [ 0; 1; 2; 3; 5 ] ~f:(fun k ->
          Printf.sprintf "%g" (Timing_library.Wire_load.cap_pf small ~fanout:k))));
  let big = Timing_library.find_wire_load_exn t "big" in
  printf "big: %g %g pF\n"
    (Timing_library.Wire_load.cap_pf big ~fanout:1)
    (Timing_library.Wire_load.cap_pf big ~fanout:4);
  (* fractional fanouts kept: 10 + 20 * 0.75 = 25 fF at fanout 2 *)
  printf "frac: %g pF\n"
    (Timing_library.Wire_load.cap_pf (Timing_library.find_wire_load_exn t "frac") ~fanout:2);
  (* greatest lower bound: 100 is "big"; below every range, the lowest
     (declared last) *)
  List.iter [ -1.; 50.; 100.; 5000. ] ~f:(fun a ->
    printf "area %g -> %s\n" a (Timing_library.wire_load_for_area t a).name);
  let f = Timing_library.find_exn t "f" in
  let pw = Option.value_exn (Timing_library.Cell.pulse_width f ~input:"CK") in
  let look tbl s = Timing_library.Pulse_width.lookup (Option.value_exn tbl) ~slew:s in
  (* high: the 50 ps attribute wins over the 100-120 ps table; low: 70-90 ps
     table, 80 at 20 ps slew *)
  printf "CK min high %.3f ns, min low at 20 ps slew %.3f ns; D has none: %b\n"
    (look pw.high 0.02) (look pw.low 0.02)
    (Option.is_none (Timing_library.Cell.pulse_width f ~input:"D"));
  List.iter (Timing_library.Cell.constraints f ~input:"RN") ~f:(fun a ->
    printf "RN constraint: %s vs %s, at (20, 20) ps (centre of 1..4 ps): %.4f ns\n" a.timing_type a.related_pin
      (Timing_library.Table.lookup (Option.value_exn a.rise_constraint) 0.02 0.02));
  printf "no delay arcs from constraint groups: %b\n" (List.is_empty (Timing_library.Cell.arcs f ~output:"Q"));
  [%expect {|
    small: 0 0.002 0.006 0.01 0.022 pF
    big: 0.01 0.04 pF
    frac: 0.025 pF
    area -1 -> small
    area 50 -> small
    area 100 -> big
    area 5000 -> big
    CK min high 0.050 ns, min low at 20 ps slew 0.080 ns; D has none: true
    RN constraint: recovery_rising vs CK, at (20, 20) ps (centre of 1..4 ps): 0.0025 ns
    no delay arcs from constraint groups: true
    |}]
;;
