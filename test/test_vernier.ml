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
let reference ~a ~scale ~offset ~ratio ~(mode : Vernier.Mode.t) =
  match mode with
  | Fine -> 0, a % 16, false
  | Bypass | Coarse | Full ->
    if scale = 0
    then Int.min a 39, 0, false
    else (
      let p = (((a * scale) + 32) / 64) + offset in
      let c = p / 64 in
      let f = (((p % 64) * (16 + ratio)) + 64) / 128 in
      let c_over = c > 39
      and f_over = f > 15 in
      let coarse = if c_over then 39 else c in
      match mode with
      | Coarse -> coarse, 0, c_over
      | Full -> coarse, (if c_over || f_over then 15 else f), c_over || f_over
      | Bypass | Fine -> coarse, (if c_over || f_over then 15 else f), false)
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
    let scale =
      match k % 8 with
      | 0 -> 0
      | 1 | 2 -> Random.State.int rng 1024
      | _ -> 200 + Random.State.int rng 500
    in
    let offset = Random.State.int rng 256 in
    let ratio = Random.State.int rng 16 in
    let mode = List.nth_exn Vernier.Mode.all (Random.State.int rng 4) in
    set i.V.Map.I.fraction 8 a;
    set i.scale V.scale_bits scale;
    set i.offset Vernier.offset_bits offset;
    set i.ratio Vernier.ratio_bits ratio;
    set i.mode 2 (Vernier.Mode.to_int mode);
    Cyclesim.cycle sim;
    let o = Cyclesim.outputs ~clock_edge:Before sim in
    let got = Bits.to_int_trunc !(o.V.Map.O.coarse), Bits.to_int_trunc !(o.fine), Bits.to_bool !(o.range_fault) in
    if not ([%equal: int * int * bool] got (reference ~a ~scale ~offset ~ratio ~mode)) then Int.incr mismatches
  done;
  printf "%d random inputs, mismatches %d\n" n !mismatches;
  [%expect {| 50000 random inputs, mismatches 0 |}]
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
    (* the model's ideal constants: 4096 / 170 coarse steps per cycle, R = 10 *)
    List.iter i.scale ~f:(fun p -> set p V.scale_bits (4096 * 16 / 170));
    List.iter i.offset ~f:(fun p -> set p Vernier.offset_bits 0);
    List.iter i.ratio ~f:(fun p -> set p Vernier.ratio_bits 4);
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

let%expect_test "control: latency, safe select updates, overruns, mode switches" =
  let open Sim in
  let t = create () in
  run t 2;
  t.i.clear := Bits.gnd;
  run t 2;
  printf "after clear: mode %d\n" (get t.o.active_mode 0);
  let mode m = set (List.hd_exn t.i.mode) 2 (Vernier.Mode.to_int m) in
  let identity () = List.iter t.i.scale ~f:(fun p -> set p V.scale_bits 0) in
  identity ();
  mode Full;
  run t 8;
  let edge ~level ~fraction =
    set (List.hd_exn t.i.fraction) 8 fraction;
    set (List.hd_exn t.i.level) 1 level;
    let e = t.cycle + 1 in
    step t;
    e
  in
  let report what e =
    let ts = Queue.to_list t.toggles in
    Queue.clear t.toggles;
    printf
      "%s (registered at edge %d): %s | overrun %d, fallback %d, out %d\n"
      what e
      (String.concat
         ~sep:", "
         (List.map ts ~f:(fun (c, l, k) -> Printf.sprintf "line %d toggles in cycle +%d at %d ticks" l (c - e) k)))
      (get t.o.overrun 0) (get t.o.fallback 0) (get t.o.out 0)
  in
  (* identity, full mode: coarse tap 3: 375 + 3 * 170 = 885 ticks; launch at edge e + 2 *)
  let e = edge ~level:1 ~fraction:3 in
  run t 8;
  report "rise, coarse tap 3" e;
  (* tap 39: 375 + 6630 = 7005 ticks = 1 cycle + 2909 *)
  let e = edge ~level:0 ~fraction:39 in
  run t 8;
  report "fall, coarse tap 39" e;
  (* the rise line again two cycles after its last edge, with a new tap *)
  let e = edge ~level:1 ~fraction:20 in
  ignore (edge ~level:0 ~fraction:20 : int);
  ignore (edge ~level:1 ~fraction:0 : int);
  run t 8;
  report "rise 20, fall 20, rise 0 (one cycle apart)" e;
  set (List.hd_exn t.i.clear_flags) 1 1;
  step t;
  set (List.hd_exn t.i.clear_flags) 1 0;
  (* an unchanged tap on a busy line: allowed *)
  let e = edge ~level:0 ~fraction:0 in
  ignore (edge ~level:1 ~fraction:0 : int);
  ignore (edge ~level:0 ~fraction:0 : int);
  ignore (edge ~level:1 ~fraction:0 : int);
  run t 8;
  report "edges every cycle with an unchanged tap" e;
  (* a mode change waits for both lines to be quiet *)
  let e = edge ~level:0 ~fraction:39 in
  mode Fine;
  let changed = ref None in
  for _ = 1 to 12 do
    step t;
    if Option.is_none !changed && get t.o.active_mode 0 = Vernier.Mode.to_int Fine
    then changed := Some (t.cycle - e)
  done;
  Queue.clear t.toggles;
  printf "mode change requested after an edge registered at %d: applied %d edges later (launch at +2, settle 4)\n"
    e (Option.value_exn !changed);
  (* fine mode: the fraction's low bits are the fine tap: 375 + 7 * 17 = 494 *)
  let e = edge ~level:1 ~fraction:(16 + 7) in
  run t 8;
  report "fine mode, fraction 23" e;
  (* a range fault: 64 coarse steps per cycle, a line of 40 taps (as at a
     clock slower than the line) *)
  mode Full;
  List.iter t.i.scale ~f:(fun p -> set p V.scale_bits 1023);
  run t 8;
  let e = edge ~level:0 ~fraction:255 in
  run t 8;
  printf "range fault (registered at edge %d): range_fault %d, fallback %d, active mode %d\n" e (get t.o.range_fault 0)
    (get t.o.fallback 0) (get t.o.active_mode 0);
  Queue.clear t.toggles;
  [%expect {|
    after clear: mode 0
    rise, coarse tap 3 (registered at edge 13): line 0 toggles in cycle +2 at 885 ticks | overrun 0, fallback 0, out 1
    fall, coarse tap 39 (registered at edge 22): line 1 toggles in cycle +3 at 2909 ticks | overrun 0, fallback 0, out 0
    rise 20, fall 20, rise 0 (one cycle apart) (registered at edge 31): line 0 toggles in cycle +2 at 3775 ticks, line 1 toggles in cycle +3 at 3775 ticks, line 0 toggles in cycle +4 at 3775 ticks | overrun 1, fallback 1, out 1
    edges every cycle with an unchanged tap (registered at edge 43): line 1 toggles in cycle +2 at 375 ticks, line 0 toggles in cycle +3 at 375 ticks, line 1 toggles in cycle +4 at 375 ticks, line 0 toggles in cycle +5 at 375 ticks | overrun 0, fallback 0, out 1
    mode change requested after an edge registered at 55: applied 6 edges later (launch at +2, settle 4)
    fine mode, fraction 23 (registered at edge 68): line 0 toggles in cycle +2 at 494 ticks | overrun 0, fallback 0, out 1
    range fault (registered at edge 85): range_fault 1, fallback 1, active mode 3
    |}]
;;

let%expect_test "control: mapped fractions on the ideal model land at a / 256 of a cycle" =
  let open Sim in
  let t = create () in
  run t 2;
  t.i.clear := Bits.gnd;
  set (List.hd_exn t.i.mode) 2 (Vernier.Mode.to_int Full);
  (* offset 0: the model's base delay is d_align *)
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
  [%expect {| worst error 13 ticks (fine step 17) |}]
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
  [%expect {|
    sg13cmos5l_buf_1             60
    sg13cmos5l_dfrbpq_1           6
    sg13cmos5l_dlygate4sd3_1    312
    sg13cmos5l_inv_1            320
    sg13cmos5l_mux2_1           234
    sg13cmos5l_xor2_1             2
    |}]
;;

let%expect_test "Sdc.vernier: false paths through the XOR outputs; dont-touch covers the lines" =
  let sdc = Sdc.create (circuit ()) |> Sdc.vernier ~name:"vernier" ~pins:2 in
  print_string (Sdc.sdc sdc ~stage:Pnr ~system_clock:"clk");
  let nets = Sdc.net_names sdc in
  printf "protected nets: %d, e.g. %s\n" (List.length nets)
    (String.concat ~sep:" " (List.filter nets ~f:(fun n -> String.is_prefix n ~prefix:"vernier_p0_r_cm6")));
  [%expect {|
    # hardcaml_clockless islands (PnR): append after the system clock is created.
    set_false_path -through [get_pins {vernier_p0_xor/X}]
    set_false_path -through [get_pins {vernier_p1_xor/X}]
    protected nets: 942, e.g. vernier_p0_r_cm6_0__X
    |}]
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
  [%expect {|
    ("Vernier: bad configuration" (msg "coarse_taps must be 2..64"))
    ("Vernier: bad configuration" (msg "fraction_bits must be 3..12"))
    ("Vernier.Model is simulation-only: use it in a scope with ~flatten_design:true"
     (module_name v))
    ("delay line stage inverts"
     (stage (sg13cmos5l_dlygate4sd3_1 sg13cmos5l_inv_1)))
    |}]
;;
