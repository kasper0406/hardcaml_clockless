(* Event_sim throughput: a NAND + 8-stage ring (~1.5 ns period, delay cells
   and inverters) sampled by 9 dfrbpq flops on a 50 MHz clock, plus a
   16-bit Hardcaml counter clocked by the ring, for 10 us of simulated time.

   dune exec bench/ring_bench.exe -- [LIBERTY] [US] *)

open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless
module Sim = Event_sim

let () =
  let argv = Sys.get_argv () in
  let path = if Array.length argv > 1 then argv.(1) else "test/data/sg13cmos5l_excerpt.lib" in
  let us = if Array.length argv > 2 then Float.of_string argv.(2) else 10. in
  let t0 = Unix.gettimeofday () in
  let tl = Timing_library.load path in
  let lib = tl.library in
  let t1 = Unix.gettimeofday () in
  let en = Signal.input "en" 1
  and clk = Signal.input "clk" 1
  and rst_n = Signal.input "rst_n" 1 in
  let fb = Signal.wire 1 in
  let nand = Cell.instantiate lib ~cell:"sg13cmos5l_nand2_1" ~instance:"ring_s0" [ "A", en; "B", fb ] "Y" in
  let nodes =
    List.fold (List.init 8 ~f:Fn.id) ~init:[ nand ] ~f:(fun acc i ->
      let prev = List.hd_exn acc in
      let cell, out = if i % 2 = 0 then "sg13cmos5l_dlygate4sd1_1", "X" else "sg13cmos5l_inv_1", "Y" in
      Cell.instantiate lib ~cell ~instance:(Printf.sprintf "ring_s%d" (i + 1)) [ "A", prev ] out :: acc)
    |> List.rev
  in
  Signal.assign fb (List.last_exn nodes);
  let qs =
    List.mapi nodes ~f:(fun i d ->
      Signal.output
        (Printf.sprintf "q%d" i)
        (Cell.instantiate lib ~cell:"sg13cmos5l_dfrbpq_1" ~instance:(Printf.sprintf "smp%d" i)
           [ "CLK", clk; "D", d; "RESET_B", rst_n ] "Q"))
  in
  let count =
    Signal.reg_fb (Signal.Reg_spec.create ~clock:(List.last_exn nodes) ()) ~width:16 ~f:(fun q ->
      Signal.(q +:. 1))
  in
  let circuit = Circuit.create_exn ~name:"bench" (Signal.output "count" count :: qs) in
  let sim = Sim.create tl circuit in
  Sim.set_input sim "rst_n" V1 ~at:0;
  Sim.set_input sim "en" V0 ~at:0;
  Sim.set_input sim "en" V1 ~at:(Sim.of_ns 10.);
  Sim.add_clock sim "clk" ~period:(Sim.of_ns 20.) ~start:(Sim.of_ns 20.);
  let t2 = Unix.gettimeofday () in
  Sim.run_until sim (Sim.of_ns (us *. 1000.));
  let t3 = Unix.gettimeofday () in
  printf "liberty load %.3fs, build %.3fs, run %.3fs for %.1f us simulated\n" (t1 -. t0) (t2 -. t1) (t3 -. t2) us;
  printf "events %d (%.2f M/s), counter %s, metastable captures %d\n" (Sim.events_processed sim)
    (Float.of_int (Sim.events_processed sim) /. (t3 -. t2) /. 1e6)
    (Option.value_map (Sim.to_int sim "count") ~default:"X" ~f:Int.to_string)
    (List.length (Sim.metastable_events sim))
;;
