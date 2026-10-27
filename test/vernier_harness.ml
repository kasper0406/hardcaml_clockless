(* Event_sim harness for Vernier: the chip configuration (2 pins, 40 coarse
   taps of dlygate4sd3/inv/dlygate4sd3/inv, 16 fine taps of buf_1), with a
   dfrbpq "sink" flop clocked by each output, so Event_sim's pulse-width
   check sees every pulse on the pad net. *)

open! Base
open Hardcaml
open Hardcaml_clockless

let corners =
  [ "typ", "data/sg13cmos5l_excerpt.lib"
  ; "slow", "data/sg13cmos5l_excerpt_slow_1p08V_125C.lib"
  ; "fast", "data/sg13cmos5l_excerpt_fast_1p32V_m40C.lib"
  ]
;;

let pins = 2
let coarse_taps = 40
let fine_taps = 16
let period = Event_sim.of_ns 20.
let period_ps = Event_sim.to_ps period

module V = Vernier.Make (struct
    let pins = pins
    let coarse_taps = coarse_taps
    let fine_taps = fine_taps
    let fraction_bits = 8
  end)

let dly = "sg13cmos5l_dlygate4sd3_1"
let inv = "sg13cmos5l_inv_1"
let coarse_stage = [ dly; inv; dly; inv ]
let fine_stage = [ "sg13cmos5l_buf_1" ]
let flop : Vernier.flop = { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }

let cells library : Vernier.cells =
  { library
  ; coarse_stage
  ; fine_stage
  ; inverter = inv
  ; mux2 = "sg13cmos5l_mux2_1"
  ; xor2 = "sg13cmos5l_xor2_1"
  ; flop
  }
;;

let circuit library =
  let scope = Scope.create ~flatten_design:true () in
  let i = V.I.Of_signal.inputs () in
  let o = V.create (Cells (cells library)) scope i in
  let sinks =
    List.mapi o.out ~f:(fun p out ->
      let q =
        Cell.instantiate
          library
          ~cell:flop.cell
          ~instance:(Printf.sprintf "sink%d" p)
          [ "CLK", out; "D", Signal.gnd; "RESET_B", Signal.vdd ]
          "Q"
      in
      Signal.output (Printf.sprintf "sink%d" p) q)
  in
  Circuit.create_exn ~name:"vernier_timing" (V.O.Of_signal.outputs o |> V.O.to_list |> fun l -> l @ sinks)
;;

let oracle path : Vernier_oracle.t =
  { lib = Oracle.load path
  ; wire_model = "1k"
  ; coarse_taps
  ; fine_taps
  ; coarse_stage
  ; fine_stage
  ; inverter = inv
  ; mux2 = "sg13cmos5l_mux2_1"
  ; xor2 = "sg13cmos5l_xor2_1"
  ; flop = flop.cell
  ; clock_slew = Event_sim.Config.default.input_slew
  ; out_extra = [ flop.cell, "CLK" ]
  ; xor_extra_fanout = 1 (* the loopback synchroniser *)
  }
;;

let base_config = { Event_sim.Config.default with wire_load = Liberty_default }

(* Pre-layout realism: 1k wire load, 3% per-instance delay variation, clock
   insertion with skew, pulse-width and recovery/removal checks logged. The
   control's (Hardcaml) registers switch 0.5 ns after the ideal edge: clock
   insertion plus a flop's clk->Q, so the hand-placed flops, whose clock pins
   see the insertion delay, are not handed a hold violation. *)
let realistic_config ~seed =
  { base_config with
    variation_sigma = 0.03
  ; reg_clock_to_q = Event_sim.of_ns 0.5
  ; seed
  ; clock_insertion = 0.3
  ; clock_skew_sigma = 0.03
  ; pulse_width_check = Log
  ; recovery_removal_check = Log
  }
;;

type constants =
  { scale : int array
  ; offset : int array
  ; ratio : int array
  }

(* scale 0: the fraction is the coarse tap (fine mode: the fine tap) *)
let identity =
  { scale = Array.create ~len:(2 * pins) 0
  ; offset = Array.create ~len:(2 * pins) 0
  ; ratio = Array.create ~len:(2 * pins) 0
  }
;;

let of_calibration (c : Vernier.Calibration.t) = { scale = c.scale; offset = c.offset; ratio = c.ratio }

type t =
  { sim : Event_sim.t
  ; edges : (int * Event_sim.Value.t) Queue.t array (* per pin: output changes *)
  ; mutable level : int array (* requested *)
  }

let set t name v = Event_sim.set_input_int t.sim name v ~at:(Event_sim.now t.sim)

let clear_flags t ~pin v = set t (Printf.sprintf "clear_flags_%d" pin) v

let set_constants t (c : constants) =
  Array.iteri c.scale ~f:(fun l v -> set t (Printf.sprintf "scale_%d" l) v);
  Array.iteri c.offset ~f:(fun l v -> set t (Printf.sprintf "offset_%d" l) v);
  Array.iteri c.ratio ~f:(fun l v -> set t (Printf.sprintf "ratio_%d" l) v)
;;

(* Run to the middle of cycle [k] (inputs set there are registered at edge
   [k + 1]). *)
let cycle t = Event_sim.now t.sim / period
let goto t k = Event_sim.run_until t.sim ((k * period) + (period / 2))
let wait t n = goto t (cycle t + n)

let set_mode t ~pin (m : Vernier.Mode.t) = set t (Printf.sprintf "mode_%d" pin) (Vernier.Mode.to_int m)

let create ?(config = base_config) ?(settle = 4) ?(constants = identity) ?(mode = Vernier.Mode.Full) path =
  let tl = Timing_library.load path in
  let sim = Event_sim.create ~config tl (circuit tl.library) in
  let t = { sim; edges = Array.init pins ~f:(fun _ -> Queue.create ()); level = Array.create ~len:pins 0 } in
  set t "clear" 1;
  for p = 0 to pins - 1 do
    set t (Printf.sprintf "level_%d" p) 0;
    set t (Printf.sprintf "fraction_%d" p) 0;
    set_mode t ~pin:p mode;
    set t (Printf.sprintf "clear_flags_%d" p) 0
  done;
  set t "settle_cycles" settle;
  set_constants t constants;
  Event_sim.add_clock sim "clock" ~period;
  for p = 0 to pins - 1 do
    Event_sim.on_change sim (Printf.sprintf "out_%d" p) (fun ~time v -> Queue.enqueue t.edges.(p) (time, v))
  done;
  goto t 2;
  set t "clear" 0;
  wait t 10;
  for p = 0 to pins - 1 do
    Queue.clear t.edges.(p)
  done;
  t
;;

(* Request a level on [pin] at the current mid-cycle [k]: the edge is
   launched at clock edge [k + 3]. Returns that edge's time. *)
let request t ~pin ~level ?(fraction = 0) () =
  set t (Printf.sprintf "level_%d" pin) level;
  set t (Printf.sprintf "fraction_%d" pin) fraction;
  t.level.(pin) <- level;
  (cycle t + 3) * period
;;

let flag t name pin = Event_sim.to_int t.sim (Printf.sprintf "%s_%d" name pin)

(* Output changes on [pin] in [lo, hi). *)
let edges_between t ~pin lo hi =
  Queue.to_list t.edges.(pin) |> List.filter ~f:(fun (time, _) -> time >= lo && time < hi)
;;
