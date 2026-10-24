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
