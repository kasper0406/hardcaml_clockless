(* Vernier in Cyclesim: the fraction -> (coarse, fine) mapping against an
   independent integer reference, the control (latency, safe select
   updates, rate violations, mode switches) on the behavioural Model, the
   netlist structure and the Sdc constraints. Gate-level timing is in
   test_vernier_timing.ml. *)

open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless

module V = Vernier.Make (struct
    let pins = 2
    let coarse_taps = 40
    let fine_taps = 16
    let fraction_bits = 8
  end)

(* The mapping as specified in vernier.mli, written out with integers. *)
let reference ~a ~scale ~offset ~skew ~apply ~ratio ~(mode : Vernier.Line_mode.t) ~raw ~rc ~rf =
  if raw
  then Int.min rc 39, Int.min rf 15, false
  else (
    let p = ((a * scale) + 32) / 64 + offset + if apply then skew else 0 in
    let p = Int.max 0 p in
    let fine x = ((x * (16 + ratio)) + 64) / 128 in
    match mode with
    | Full ->
      let c = p / 64 in
      let f = fine (p % 64) in
      if c > 39 then 39, 15, true else if f > 15 then c, 15, true else c, f, false
    | Coarse_only ->
      let c = (p + 32) / 64 in
      if c > 39 then 39, 0, true else c, 0, false
    | Fine_only ->
      let x = Int.min p 127 in
      let f = fine x in
      if f > 15 then 0, 15, true else 0, f, p > 127
    | Direct -> 0, 0, false)
;;

let%expect_test "mapping: hardware equals the integer reference" =
  let module C = Cyclesim.With_interface (V.Map.I) (V.Map.O) in
  let sim = C.create (fun i -> V.Map.create i) in
  let i = Cyclesim.inputs sim in
  let set (p : Bits.t ref) w v = p := Bits.of_int_trunc ~width:w v in
  let rng = Random.State.make [| 1 |] in
  let mismatches = ref 0 in
  let n = 50_000 in
  for k = 1 to n do
    let a = Random.State.int rng 256 in
    let scale = if k % 4 = 0 then Random.State.int rng 1024 else 200 + Random.State.int rng 500 in
    let offset = Random.State.int rng 256 in
    let skew = Random.State.int rng 128 - 64 in
    let apply = Random.State.bool rng in
    let ratio = Random.State.int rng 16 in
    let mode = List.nth_exn Vernier.Line_mode.all (Random.State.int rng 4) in
    let raw = Random.State.int rng 8 = 0 in
    let rc = Random.State.int rng 64
    and rf = Random.State.int rng 16 in
    set i.V.Map.I.fraction 8 a;
    set i.scale V.scale_bits scale;
    set i.offset Vernier.offset_bits offset;
    set i.skew Vernier.skew_bits skew;
    set i.apply_skew 1 (Bool.to_int apply);
    set i.ratio Vernier.ratio_bits ratio;
    set i.mode 2 (Vernier.Line_mode.to_int mode);
    set i.raw 1 (Bool.to_int raw);
    set i.raw_coarse V.coarse_bits rc;
    set i.raw_fine V.fine_bits rf;
    Cyclesim.cycle sim;
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    let got = Bits.to_int_trunc !(o.V.Map.O.coarse), Bits.to_int_trunc !(o.fine), Bits.to_bool !(o.saturated) in
    let want = reference ~a ~scale ~offset ~skew ~apply ~ratio ~mode ~raw ~rc ~rf in
    if not ([%equal: int * int * bool] got want) then Int.incr mismatches
  done;
  printf "%d random inputs, mismatches %d\n" n !mismatches;
  [%expect {| |}]
;;

(* --- Control, on the behavioural model --- *)

(* 4096 ticks per cycle; at 50 MHz: 1.83 ns base, 830 ps coarse, 85 ps fine. *)
let model : Vernier.model = { base_ticks = 375; coarse_ticks = 170; fine_ticks = 17 }

module Sim = struct
  type t =
    { sim : Cyclesim.With_interface(V.I)(V.O).t
    ; i : Bits.t ref V.I.t
    ; o : Bits.t ref V.O.t
    ; mutable cycle : int
    ; toggles : (int * int * int) Queue.t (* cycle, line, ticks *)
    }

  let create () =
    let module C = Cyclesim.With_interface (V.I) (V.O) in
    let scope = Scope.create ~flatten_design:true () in
    let sim = C.create (V.create (Model model) scope) in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    let t = { sim; i; o; cycle = 0; toggles = Queue.create () } in
    let set p w v = p := Bits.of_int_trunc ~width:w v in
    set i.settle_cycles Vernier.settle_bits 4;
    List.iter i.scale ~f:(fun p -> set p V.scale_bits (4096 * 16 / 170));
    List.iter i.offset ~f:(fun p -> set p Vernier.offset_bits 0);
    set i.ratio Vernier.ratio_bits 4;
    i.clear := Bits.vdd;
    t
  ;;

  let set p w v = p := Bits.of_int_trunc ~width:w v

  let step t =
    Cyclesim.cycle t.sim;
    t.cycle <- t.cycle + 1;
    List.iteri t.o.model_toggle ~f:(fun l x ->
      if Bits.to_bool !x then Queue.enqueue t.toggles (t.cycle, l, Bits.to_int_trunc !(List.nth_exn t.o.model_ticks l)))
  ;;

  let run t n =
    for _ = 1 to n do
      step t
    done
  ;;

  let get l p = Bits.to_int_trunc !(List.nth_exn l p)
end

let%expect_test "control: latency, safe select updates, rate violations, mode switches" =
  let open Sim in
  let t = create () in
  run t 2;
  t.i.clear := Bits.gnd;
  set (List.hd_exn t.i.bypass) 1 0;
  run t 10;
  printf "after clear: bypass %d mode %d\n" (get t.o.active_bypass 0) (get t.o.active_mode 0);
  set t.i.raw 1 1;
  let edge ~level ~coarse ~fine =
    set t.i.raw_coarse V.coarse_bits coarse;
    set t.i.raw_fine V.fine_bits fine;
    set (List.hd_exn t.i.level) 1 level;
    let e = t.cycle + 1 in
    step t;
    e
  in
  let report what e =
    let d = model.base_ticks in
    ignore d;
    let ts = Queue.to_list t.toggles in
    Queue.clear t.toggles;
    printf
      "%s (registered at edge %d): %s | rate_violation %d\n"
      what e
      (String.concat
         ~sep:", "
         (List.map ts ~f:(fun (c, l, k) -> Printf.sprintf "line %d toggles in cycle +%d at %d ticks" l (c - e) k)))
      (get t.o.rate_violation 0)
  in
  (* (3, 2): 375 + 3*170 + 2*17 = 919 ticks; launch at edge e + 2 *)
  let e = edge ~level:1 ~coarse:3 ~fine:2 in
  run t 8;
  report "rise, code (3, 2)" e;
  (* (39, 15): 375 + 6630 + 255 = 7260 ticks = 1 cycle + 3164 *)
  let e = edge ~level:0 ~coarse:39 ~fine:15 in
  run t 8;
  report "fall, code (39, 15)" e;
  (* rise line again 2 cycles after its last launch, new code: too early *)
  let e = edge ~level:1 ~coarse:20 ~fine:0 in
  ignore (edge ~level:0 ~coarse:20 ~fine:0 : int);
  let e2 = edge ~level:1 ~coarse:0 ~fine:0 in
  run t 8;
  report "rise (20,0), fall (20,0), rise (0,0) two cycles apart" e;
  ignore e2;
  (* same code on a busy line: allowed *)
  set t.i.clear_flags 1 1;
  step t;
  set t.i.clear_flags 1 0;
  let e = edge ~level:0 ~coarse:0 ~fine:0 in
  ignore (edge ~level:1 ~coarse:0 ~fine:0 : int);
  ignore (edge ~level:0 ~coarse:0 ~fine:0 : int);
  ignore (edge ~level:1 ~coarse:0 ~fine:0 : int);
  run t 8;
  report "edges every cycle with an unchanged code" e;
  (* a mode change waits for both lines to be quiet *)
  let e = edge ~level:0 ~coarse:39 ~fine:0 in
  set (List.hd_exn t.i.line_mode) 2 (Vernier.Line_mode.to_int Coarse_only);
  set (List.hd_exn t.i.bypass) 1 1;
  let changed = ref None in
  for _ = 1 to 12 do
    step t;
    if Option.is_none !changed && get t.o.active_mode 0 = 1 then changed := Some (t.cycle - e)
  done;
  Queue.clear t.toggles;
  printf "mode change requested after an edge registered at %d: applied %d edges later (launch at +2, settle 4), bypass %d\n"
    e (Option.value_exn !changed) (get t.o.active_bypass 0);
  [%expect {| |}]
;;

let%expect_test "control: mapped fractions on the ideal model land at a / 256 of a cycle" =
  let open Sim in
  let t = create () in
  run t 2;
  t.i.clear := Bits.gnd;
  set (List.hd_exn t.i.bypass) 1 0;
  (* offset: the model's base delay is the reference *)
  run t 10;
  let worst = ref 0 in
  for a = 0 to 255 do
    set (List.hd_exn t.i.fraction) 8 a;
    set (List.hd_exn t.i.level) 1 (1 - get t.i.level 0);
    let launch = t.cycle + 2 in
    run t 8;
    match Queue.to_list t.toggles with
    | [ (c, _, k) ] ->
      Queue.clear t.toggles;
      let ticks = ((c - launch - 1) * 4096) + k in
      let want = model.base_ticks + (a * 16) in
      worst := Int.max !worst (abs (ticks - want))
    | _ -> failwith "expected one toggle"
  done;
  printf "worst error %d ticks (fine step %d)\n" !worst model.fine_ticks;
  [%expect {| |}]
;;

(* --- Structure and constraints --- *)

let lib = lazy (Timing_library.load "data/sg13cmos5l_excerpt.lib")

let cells () : Vernier.cells =
  { library = (force lib).library
  ; coarse_stage = [ "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1"; "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1" ]
  ; fine_stage = [ "sg13cmos5l_buf_1" ]
  ; inverter = "sg13cmos5l_inv_1"
  ; mux2 = "sg13cmos5l_mux2_1"
  ; xor2 = "sg13cmos5l_xor2_1"
  ; flop = { cell = "sg13cmos5l_dfrbpq_1"; clk = "CLK"; d = "D"; reset_n = "RESET_B"; q = "Q" }
  }
;;

let circuit () =
  let module C = Circuit.With_interface (V.I) (V.O) in
  C.create_exn ~name:"vernier_top" (V.create (Cells (cells ())) (Scope.create ~flatten_design:true ()))
;;

let%expect_test "netlist: hand-placed cells per pin, stable names" =
  let sdc = Sdc.create (circuit ()) in
  let by_cell = Hashtbl.create (module String) in
  List.iter (Sdc.instances sdc) ~f:(fun i -> Hashtbl.incr by_cell i.cell);
  Hashtbl.to_alist by_cell
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.iter ~f:(fun (c, n) -> printf "%-26s %4d\n" c n);
  let names = List.map (Sdc.instances sdc) ~f:(fun i -> i.name) in
  List.iter
    [ "vernier_p0_r_launch"; "vernier_p0_r_c_stage0_0"; "vernier_p0_r_c_stage38_3"; "vernier_p0_r_cm6_0"
    ; "vernier_p0_r_cinv"; "vernier_p0_r_cbinv"; "vernier_p0_r_cbyp"; "vernier_p0_r_f_stage14_0"
    ; "vernier_p0_r_fm4_0"; "vernier_p0_r_fbyp"; "vernier_p1_f_fbyp"; "vernier_p0_xor"; "vernier_p0_level"
    ; "vernier_p0_out" ]
    ~f:(fun n -> if not (List.mem names n ~equal:String.equal) then printf "missing %s\n" n);
  [%expect {| |}]
;;

let%expect_test "Sdc.vernier: false paths through the XOR outputs; dont-touch covers the lines" =
  let sdc = Sdc.create (circuit ()) |> Sdc.vernier ~name:"vernier" ~pins:2 in
  print_string (Sdc.sdc sdc ~stage:Pnr ~system_clock:"clk");
  let nets = Sdc.net_names sdc in
  printf "protected nets: %d, e.g. %s\n" (List.length nets)
    (String.concat ~sep:" " (List.filter nets ~f:(fun n -> String.is_prefix n ~prefix:"vernier_p0_r_cm6")));
  [%expect {| |}]
;;

let%expect_test "configuration errors" =
  let try_ f = match f () with _ -> print_endline "ok" | exception e -> print_s (Exn.sexp_of_t e) in
  try_ (fun () ->
    let module _ = Vernier.Make (struct let pins = 2 let coarse_taps = 1 let fine_taps = 16 let fraction_bits = 8 end) in
    ());
  try_ (fun () ->
    let module _ = Vernier.Make (struct let pins = 2 let coarse_taps = 40 let fine_taps = 16 let fraction_bits = 2 end) in
    ());
  try_ (fun () ->
    let module C = Circuit.With_interface (V.I) (V.O) in
    C.create_exn ~name:"x" (V.hierarchical ~module_name:"v" (Model model) (Scope.create ~flatten_design:false ())));
  try_ (fun () ->
    Delay_line.create_stages (force lib).library ~name:"d" ~stage:[ "sg13cmos5l_dlygate4sd3_1"; "sg13cmos5l_inv_1" ] ~length:2 Signal.gnd);
  [%expect {| |}]
;;
