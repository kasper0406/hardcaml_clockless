open! Base
open Stdio
open Hardcaml
open Hardcaml_clockless
module Sim = Event_sim

let typ_path = "data/sg13cmos5l_excerpt.lib"
let slow_path = "data/sg13cmos5l_excerpt_slow_1p08V_125C.lib"
let fast_path = "data/sg13cmos5l_excerpt_fast_1p32V_m40C.lib"
let tlib = lazy (Timing_library.load typ_path)
let olib = lazy (Oracle.load typ_path)
let wire_cap = Sim.Config.default.wire_cap
let input_slew = Sim.Config.default.input_slew

(* --- Rings ------------------------------------------------------------------- *)

type stage =
  { cell : string
  ; inp : string
  ; out : string
  ; inverting : bool
  }

let nand = { cell = "sg13cmos5l_nand2_1"; inp = "B"; out = "Y"; inverting = true }
let inv = { cell = "sg13cmos5l_inv_1"; inp = "A"; out = "Y"; inverting = true }
let dly = { cell = "sg13cmos5l_dlygate4sd1_1"; inp = "A"; out = "X"; inverting = false }

(* NAND enable stage, then [rest]; closed into a loop through a [Signal.wire]. *)
let ring_stages rest = nand :: rest
let fast_ring = ring_stages (List.init 8 ~f:(fun _ -> inv))
let slow_ring = ring_stages (List.concat (List.init 4 ~f:(fun _ -> [ dly; inv ])))
let node_name i (s : stage) = Printf.sprintf "ring_s%d/%s" i s.out

let build_ring (lib : Cell_library.t) stages ~en =
  let fb = Signal.wire 1 in
  let nodes =
    List.foldi stages ~init:[] ~f:(fun i acc s ->
      let prev =
        match acc with
        | [] -> fb
        | p :: _ -> p
      in
      let side = if String.equal s.cell nand.cell then [ "A", en ] else [] in
      Cell.instantiate lib ~cell:s.cell ~instance:(Printf.sprintf "ring_s%d" i) ((s.inp, prev) :: side) s.out
      :: acc)
    |> List.rev
  in
  Signal.assign fb (List.last_exn nodes);
  Array.of_list nodes
;;

(* Independent steady-state period: follow one wavefront around the ring
   (2N hops = one period) until the slews converge, summing table delays.
   [extra_load.(i)] is load on node [i] beyond the next stage's input pin. *)
let ring_oracle ?(corner = 1.0) ?(factors = fun _ -> 1.0) ?(extra_load = fun _ -> 0.) olib stages =
  let stages = Array.of_list stages in
  let n = Array.length stages in
  let load i =
    let next = stages.((i + 1) % n) in
    wire_cap +. Oracle.pin_cap olib ~cell:next.cell ~pin:next.inp +. extra_load i
  in
  let slew = ref input_slew
  and in_rise = ref true
  and period = ref 0
  and hops = ref [] in
  for _lap = 1 to 30 do
    period := 0;
    hops := [];
    for k = 0 to (2 * n) - 1 do
      let i = k % n in
      let s = stages.(i) in
      let out_rise = if s.inverting then not !in_rise else !in_rise in
      let t = Oracle.timing olib ~cell:s.cell ~out:s.out ~related:s.inp () in
      let d, s' = Oracle.arc t ~rise:out_rise ~slew:!slew ~load:(load i) in
      let f = corner *. factors i in
      let d = Oracle.fs (d *. f) in
      period := !period + d;
      hops := (i, out_rise, d) :: !hops;
      slew := s' *. f;
      in_rise := out_rise
    done
  done;
  !period, List.rev !hops
;;

(* Measured period (fs): mean over rising edges of [node] after [skip] edges. *)
let measure_period sim node ~from ~until =
  let rises = ref [] in
  Sim.on_change sim node (fun ~time v ->
    if Sim.Value.equal v V1 && time >= from then rises := time :: !rises);
  Sim.run_until sim until;
  let r = List.rev !rises in
  let n = List.length r in
  if n < 2 then None else Some ((List.last_exn r - List.hd_exn r) / (n - 1), n)
;;

let ring_circuit ?(name = "ring") lib stages =
  let en = Signal.input "en" 1 in
  let nodes = build_ring lib stages ~en in
  Circuit.create_exn ~name [ Signal.output "ring_out" nodes.(Array.length nodes - 1) ]
;;

(* --- Tests ------------------------------------------------------------------- *)

let%expect_test "chain: arrival times are sums of independently interpolated table delays" =
  let tl = force tlib
  and ol = force olib in
  let lib = tl.library in
  let a = Signal.input "a" 1
  and one = Signal.input "one" 1
  and zero = Signal.input "zero" 1 in
  (* cell, input pin, output pin, side inputs, the arc's [when] for the oracle *)
  let chain =
    [ "sg13cmos5l_inv_1", "A", "Y", [], None, true
    ; "sg13cmos5l_buf_1", "A", "X", [], None, false
    ; "sg13cmos5l_nand2_1", "B", "Y", [ "A", one ], None, true
    ; "sg13cmos5l_dlygate4sd3_1", "A", "X", [], None, false
    ; "sg13cmos5l_inv_1", "A", "Y", [], None, true
    ; "sg13cmos5l_xor2_1", "A", "X", [ "B", zero ], Some "!B", false
    ; "sg13cmos5l_nor2_1", "A", "Y", [ "B", zero ], None, true
    ; "sg13cmos5l_dlygate4sd1_1", "A", "X", [], None, false
    ]
  in
  let outs =
    List.foldi chain ~init:[] ~f:(fun i acc (cell, inp, out, side, _, _) ->
      let prev =
        match acc with
        | [] -> a
        | p :: _ -> p
      in
      Cell.instantiate lib ~cell ~instance:(Printf.sprintf "c%d" i) ((inp, prev) :: side) out :: acc)
    |> List.rev
  in
  let circuit = Circuit.create_exn ~name:"chain" [ Signal.output "y" (List.last_exn outs) ] in
  let sim = Sim.create tl circuit in
  let t_rise = Sim.of_ns 2.
  and t_fall = Sim.of_ns 10. in
  Sim.set_input sim "one" V1 ~at:0;
  Sim.set_input sim "zero" V0 ~at:0;
  Sim.set_input sim "a" V0 ~at:0;
  Sim.set_input sim "a" V1 ~at:t_rise;
  Sim.set_input sim "a" V0 ~at:t_fall;
  let arrivals = Array.create ~len:(List.length chain) [] in
  List.iteri chain ~f:(fun i (_, _, out, _, _, _) ->
    Sim.on_change sim (Printf.sprintf "c%d/%s" i out) (fun ~time _ ->
      if time >= t_rise then arrivals.(i) <- time :: arrivals.(i)));
  Sim.run_until sim (Sim.of_ns 20.);
  (* oracle *)
  let n = List.length chain in
  let chain = Array.of_list chain in
  let load i =
    if i = n - 1
    then wire_cap
    else (
      let cell, inp, _, _, _, _ = chain.(i + 1) in
      wire_cap +. Oracle.pin_cap ol ~cell ~pin:inp)
  in
  let expected t0 ~input_rises =
    let slew = ref input_slew
    and t = ref t0
    and rise = ref input_rises in
    Array.mapi chain ~f:(fun i (cell, inp, out, _, when_, inverting) ->
      let out_rise = if inverting then not !rise else !rise in
      let tg = Oracle.timing ol ~cell ~out ~related:inp ?when_ () in
      let d, s = Oracle.arc tg ~rise:out_rise ~slew:!slew ~load:(load i) in
      t := !t + Oracle.fs d;
      slew := s;
      rise := out_rise;
      !t)
  in
  let exp_r = expected t_rise ~input_rises:true
  and exp_f = expected t_fall ~input_rises:false in
  let worst = ref 0 in
  printf "stage cell                      rise-in arrival (ps)   fall-in arrival (ps)\n";
  Array.iteri chain ~f:(fun i (cell, _, _, _, _, _) ->
    match List.rev arrivals.(i) with
    | [ r; f ] ->
      worst := Int.max !worst (Int.max (abs (r - exp_r.(i))) (abs (f - exp_f.(i))));
      printf "%d     %-25s %10.3f %20.3f\n" i cell (Sim.to_ps exp_r.(i)) (Sim.to_ps exp_f.(i))
    | l -> printf "%d: unexpected %d transitions\n" i (List.length l));
  printf "max |sim - oracle| <= 1 fs per stage: %b\n" (!worst <= n);
  [%expect {|
    stage cell                      rise-in arrival (ps)   fall-in arrival (ps)
    0     sg13cmos5l_inv_1            2034.189            10035.766
    1     sg13cmos5l_buf_1            2096.002            10098.209
    2     sg13cmos5l_nand2_1          2128.314            10142.256
    3     sg13cmos5l_dlygate4sd3_1    2486.405            10516.104
    4     sg13cmos5l_inv_1            2526.396            10560.113
    5     sg13cmos5l_xor2_1           2626.893            10659.895
    6     sg13cmos5l_nor2_1           2681.292            10701.329
    7     sg13cmos5l_dlygate4sd1_1    2805.961            10840.101
    max |sim - oracle| <= 1 fs per stage: true
    |}]
;;

let%expect_test "ring oscillator: oscillates only when enabled; period matches the table oracle" =
  let tl = force tlib
  and ol = force olib in
  let circuit = ring_circuit tl.library fast_ring in
  let sim = Sim.create tl circuit in
  let n = List.length fast_ring in
  Sim.set_input sim "en" V0 ~at:0;
  let transitions = ref 0 in
  List.iteri fast_ring ~f:(fun i s ->
    Sim.on_change sim (node_name i s) (fun ~time:_ _ -> Int.incr transitions));
  Sim.run_until sim (Sim.of_ns 5.);
  let settled = !transitions in
  Sim.run_until sim (Sim.of_ns 50.);
  printf
    "disabled: nodes %s, transitions in 5..50 ns: %d\n"
    (String.of_char_list
       (List.mapi fast_ring ~f:(fun i s -> Sim.Value.to_char (Sim.value sim (node_name i s)))))
    (!transitions - settled);
  Sim.set_input sim "en" V1 ~at:(Sim.of_ns 50.);
  let period, edges =
    Option.value_exn
      (measure_period sim (node_name (n - 1) (List.last_exn fast_ring)) ~from:(Sim.of_ns 60.)
         ~until:(Sim.of_ns 1060.))
  in
  let expected, _ = ring_oracle ol fast_ring in
  let sum_stage = expected / (2 * n) in
  printf "enabled: %d rising edges in 1 us\n" edges;
  printf "oracle period %.3f ps (2 x %d stages x %.3f ps mean stage delay)\n"
    (Sim.to_ps expected) n (Sim.to_ps sum_stage);
  printf "sim/oracle - 1 within 1e-5: %b\n"
    Float.(abs ((of_int period /. of_int expected) -. 1.) < 1e-5);
  Sim.set_input sim "en" V0 ~at:(Sim.of_ns 1100.);
  Sim.run_until sim (Sim.of_ns 1110.);
  let before = !transitions in
  Sim.run_until sim (Sim.of_ns 1200.);
  printf "disabled again: transitions in 1110..1200 ns: %d\n" (!transitions - before);
  [%expect {|
    disabled: nodes 101010101, transitions in 5..50 ns: 0
    enabled: 1816 rising edges in 1 us
    oracle period 550.571 ps (2 x 9 stages x 30.587 ps mean stage delay)
    sim/oracle - 1 within 1e-5: true
    disabled again: transitions in 1110..1200 ns: 0
    |}]
;;

let%expect_test "ring period across corners: scale factor and real corner libraries" =
  let tl = force tlib
  and ol = force olib in
  let measure ?config tl =
    let circuit = ring_circuit tl.Timing_library.library fast_ring in
    let sim = Sim.create ?config tl circuit in
    Sim.set_input sim "en" V0 ~at:0;
    Sim.set_input sim "en" V1 ~at:(Sim.of_ns 5.);
    fst
      (Option.value_exn
         (measure_period sim "ring_s8/Y" ~from:(Sim.of_ns 20.) ~until:(Sim.of_ns 520.)))
  in
  let typ_sim = measure tl in
  let typ_oracle, _ = ring_oracle ol fast_ring in
  let row name ~sim ~oracle =
    printf
      "%-24s oracle %8.3f ps  x%.3f of typ   sim matches: %b\n"
      name
      (Sim.to_ps oracle)
      (Float.of_int oracle /. Float.of_int typ_oracle)
      Float.(abs ((of_int sim /. of_int oracle) -. 1.) < 1e-5)
  in
  row "typ" ~sim:typ_sim ~oracle:typ_oracle;
  List.iter [ 0.48; 0.71; 1.4; 2.1 ] ~f:(fun corner ->
    let config = { Sim.Config.default with corner } in
    row
      (Printf.sprintf "typ, corner x%.2f" corner)
      ~sim:(measure ~config tl)
      ~oracle:(fst (ring_oracle ~corner ol fast_ring)));
  let config = { Sim.Config.default with corner = 2.1; corner_scales_slew = false } in
  let sim21 = measure ~config tl in
  printf "typ, corner x2.10, slews unscaled: x%.4f of typ\n"
    (Float.of_int sim21 /. Float.of_int typ_sim);
  List.iter [ "slow 1.08V 125C", slow_path; "fast 1.32V -40C", fast_path ] ~f:(fun (name, path) ->
    row name ~sim:(measure (Timing_library.load path)) ~oracle:(fst (ring_oracle (Oracle.load path) fast_ring)));
  [%expect {|
    typ                      oracle  550.571 ps  x1.000 of typ   sim matches: true
    typ, corner x0.48        oracle  229.973 ps  x0.418 of typ   sim matches: true
    typ, corner x0.71        oracle  361.175 ps  x0.656 of typ   sim matches: true
    typ, corner x1.40        oracle  866.152 ps  x1.573 of typ   sim matches: true
    typ, corner x2.10        oracle 1637.086 ps  x2.973 of typ   sim matches: true
    typ, corner x2.10, slews unscaled: x2.1000 of typ
    slow 1.08V 125C          oracle  871.292 ps  x1.583 of typ   sim matches: true
    fast 1.32V -40C          oracle  377.579 ps  x0.686 of typ   sim matches: true
    |}]
;;

(* Ring nodes sampled by dfrbpq flops on a 50 MHz clock. *)
let sampler_circuit lib stages =
  let en = Signal.input "en" 1
  and clk = Signal.input "clk" 1
  and rst_n = Signal.input "rst_n" 1 in
  let nodes = build_ring lib stages ~en in
  let qs =
    Array.mapi nodes ~f:(fun i d ->
      Cell.instantiate
        lib
        ~cell:"sg13cmos5l_dfrbpq_1"
        ~instance:(Printf.sprintf "smp%d" i)
        [ "CLK", clk; "D", d; "RESET_B", rst_n ]
        "Q")
  in
  Circuit.create_exn
    ~name:"ring_sampler"
    (Array.to_list (Array.mapi qs ~f:(fun i q -> Signal.output (Printf.sprintf "q%d" i) q)))
;;

(* Ring state from node values: the unique stage whose output disagrees with
   its input (the wavefront in flight), and that output's current value. *)
let decode stages (q : int array) =
  let stages = Array.of_list stages in
  let n = Array.length stages in
  let bad =
    List.filter (List.init n ~f:Fn.id) ~f:(fun i ->
      let input = q.((i + n - 1) % n) in
      let want = if stages.(i).inverting then 1 - input else input in
      q.(i) <> want)
  in
  bad
;;

let%expect_test "ring sampled by 50 MHz dfrbpq flops: valid states, code density" =
  let tl = force tlib
  and ol = force olib in
  let stages = slow_ring in
  let n = List.length stages in
  let sim = Sim.create tl (sampler_circuit tl.library stages) in
  let d_cap = Oracle.pin_cap ol ~cell:"sg13cmos5l_dfrbpq_1" ~pin:"D" in
  let period, hops = ring_oracle ol stages ~extra_load:(fun _ -> d_cap) in
  printf "ring: %d stages, oracle period %.1f ps\n" n (Sim.to_ps period);
  Sim.set_input sim "rst_n" V1 ~at:0;
  Sim.set_input sim "en" V0 ~at:0;
  Sim.set_input sim "en" V1 ~at:(Sim.of_ns 10.);
  let clk_period = Sim.of_ns 20. in
  Sim.add_clock sim "clk" ~period:clk_period ~start:(Sim.of_ns 40.);
  let rises = ref [] in
  Sim.on_change sim (node_name (n - 1) (List.last_exn stages)) (fun ~time v ->
    if Sim.Value.equal v V1 && time > Sim.of_ns 20. then rises := time :: !rises);
  let cycles = 1000 in
  let hist = Array.create ~len:(2 * n) 0 in
  let clean = ref 0
  and clean_invalid = ref 0
  and meta = ref 0
  and meta_valid = ref 0
  and meta_invalid_explained = ref 0
  and meta_invalid_unexplained = ref 0
  and meta_events_seen = ref 0 in
  for c = 0 to cycles - 1 do
    let edge = Sim.of_ns 40. + (c * clk_period) in
    Sim.run_until sim (edge + (clk_period / 2));
    let events = Sim.metastable_events sim in
    let mine = List.drop events !meta_events_seen in
    meta_events_seen := List.length events;
    let m =
      List.filter_map mine ~f:(fun e ->
        if e.edge_time = edge
        then Some (Int.of_string (String.chop_prefix_exn e.instance ~prefix:"smp"))
        else None)
    in
    let q =
      Array.init n ~f:(fun i ->
        match Sim.value sim (Printf.sprintf "smp%d/Q" i) with
        | V0 -> 0
        | V1 -> 1
        | X -> failwith "unresolved")
    in
    let bad = decode stages q in
    let record () =
      match bad with
      | [ s ] -> hist.(s + (n * q.(s))) <- hist.(s + (n * q.(s))) + 1
      | _ -> ()
    in
    if List.is_empty m
    then (
      Int.incr clean;
      if List.length bad <> 1 then Int.incr clean_invalid else record ())
    else (
      Int.incr meta;
      if List.length bad = 1
      then (
        Int.incr meta_valid;
        record ())
      else if List.for_all bad ~f:(fun i ->
                List.mem m i ~equal:Int.equal || List.mem m ((i + n - 1) % n) ~equal:Int.equal)
      then Int.incr meta_invalid_explained
      else Int.incr meta_invalid_unexplained)
  done;
  printf "samples %d: clean %d (invalid %d); with metastable flops %d (valid %d, invalid next to a metastable bit %d, otherwise invalid %d)\n"
    cycles !clean !clean_invalid !meta !meta_valid !meta_invalid_explained !meta_invalid_unexplained;
  let r = List.rev !rises in
  let measured = (List.last_exn r - List.hd_exn r) / (List.length r - 1) in
  printf "sim period matches oracle (flop D pins loading the ring) within 1e-5: %b\n"
    Float.(abs ((of_int measured /. of_int period) -. 1.) < 1e-5);
  (* Expected code density: phase (s, v) lasts stage s's delay for its output
     leaving v. Metastable captures resolve at random, which blurs bins
     shorter than the setup/hold aperture (the 20 ps inverter bins); bins
     over 100 ps must be within 25% of expectation. *)
  let valid = Array.sum (module Int) hist ~f:Fn.id in
  printf "phase (stage,value)  expected  observed\n";
  let long_ok = ref true in
  Array.iteri hist ~f:(fun k obs ->
    let s = k % n
    and v = k / n in
    let d =
      List.find_map_exn hops ~f:(fun (i, out_rise, d) ->
        if i = s && Bool.equal out_rise (v = 0) then Some d else None)
    in
    let expect = Float.of_int valid *. Float.of_int d /. Float.of_int period in
    if d > Sim.of_ps 100. && Float.(abs (of_int obs -. expect) > 0.25 *. expect)
    then long_ok := false;
    printf "  (%d,%d) %-14s %8.1f %6d  %s\n" s v (String.chop_prefix_exn (List.nth_exn stages s).cell ~prefix:"sg13cmos5l_") expect obs
      (String.make (obs / 4) '#'));
  printf "every phase seen: %b; bins over 100 ps within 25%%: %b\n"
    (Array.for_all hist ~f:(fun x -> x > 0)) !long_ok;
  [%expect {|
    ring: 9 stages, oracle period 1468.0 ps
    samples 1000: clean 412 (invalid 0); with metastable flops 588 (valid 526, invalid next to a metastable bit 62, otherwise invalid 0)
    sim period matches oracle (flop D pins loading the ring) within 1e-5: true
    phase (stage,value)  expected  observed
      (0,0) nand2_1            23.9     34  ########
      (1,0) dlygate4sd1_1      85.1     79  ###################
      (2,0) inv_1              19.9     33  ########
      (3,0) dlygate4sd1_1      83.7     84  #####################
      (4,0) inv_1              19.9     35  ########
      (5,0) dlygate4sd1_1      83.7     77  ###################
      (6,0) inv_1              19.9     41  ##########
      (7,0) dlygate4sd1_1      83.7     70  #################
      (8,0) inv_1              22.7     39  #########
      (0,1) nand2_1            32.9     27  ######
      (1,1) dlygate4sd1_1      98.5     90  ######################
      (2,1) inv_1              19.9     13  ###
      (3,1) dlygate4sd1_1      94.0     96  ########################
      (4,1) inv_1              19.9     15  ###
      (5,1) dlygate4sd1_1      94.0     87  #####################
      (6,1) inv_1              19.9     16  ####
      (7,1) dlygate4sd1_1      94.0     86  #####################
      (8,1) inv_1              22.4     16  ####
    every phase seen: true; bins over 100 ps within 25%: true
    |}]
;;

let flop_circuit lib =
  let d = Signal.input "d" 1
  and clk = Signal.input "clk" 1
  and rst_n = Signal.input "rst_n" 1 in
  let q =
    Cell.instantiate lib ~cell:"sg13cmos5l_dfrbpq_1" ~instance:"ff" [ "CLK", clk; "D", d; "RESET_B", rst_n ] "Q"
  in
  Circuit.create_exn ~name:"flop" [ Signal.output "q" q ]
;;

(* Drive [d] to toggle at [edge_k + delta_k] for each cycle k, clock edges every
   10 ns; return per cycle: (delta, d rose, metastable?, Q settled, Q changes). *)
let run_flop ?config ?(d_slew = input_slew) ?(clk_slew = input_slew) deltas =
  let tl = force tlib in
  let sim = Sim.create ?config tl (flop_circuit tl.library) in
  let cyc = Sim.of_ns 10. in
  let edge k = Sim.of_ns 20. + (k * cyc) in
  Sim.set_input sim "rst_n" V1 ~at:0;
  Sim.set_input sim "d" V0 ~at:0;
  Sim.set_input sim "clk" V0 ~at:0;
  let qs = ref [] in
  Sim.on_change sim "q" (fun ~time v -> qs := (time, v) :: !qs);
  (* one cycle to load a known value *)
  Sim.set_input sim "clk" V1 ~at:(Sim.of_ns 10.) ~slew:clk_slew;
  Sim.set_input sim "clk" V0 ~at:(Sim.of_ns 15.) ~slew:clk_slew;
  Sim.run_until sim (Sim.of_ns 19.);
  List.mapi deltas ~f:(fun k delta ->
    let e = edge k in
    let rising = k % 2 = 0 in
    Sim.set_input sim "d" (if rising then V1 else V0) ~at:(e + delta) ~slew:d_slew;
    Sim.set_input sim "clk" V1 ~at:e ~slew:clk_slew;
    Sim.set_input sim "clk" V0 ~at:(e + (cyc / 2)) ~slew:clk_slew;
    qs := [];
    Sim.run_until sim (e + (cyc / 2) - 1);
    let meta =
      List.find (Sim.metastable_events sim) ~f:(fun m -> m.edge_time = e)
    in
    delta, rising, meta, Sim.value sim "q", e, List.rev !qs)
;;

let%expect_test "flop setup/hold: X only inside the liberty aperture" =
  let ol = force olib in
  let c = Oracle.constraint_group ol ~cell:"sg13cmos5l_dfrbpq_1" ~pin:"D" ~related:"CLK" in
  let sweep ~d_slew ~clk_slew ~from =
    printf "D slew %.2f ns, CLK slew %.2f ns:\n" d_slew clk_slew;
    let window ~rise =
      let kind = if rise then "rise_constraint" else "fall_constraint" in
      let s = Oracle.interp (Oracle.tbl (c ~timing_type:"setup_rising") kind) d_slew clk_slew in
      let h = Oracle.interp (Oracle.tbl (c ~timing_type:"hold_rising") kind) d_slew clk_slew in
      Oracle.fs (-.s), Oracle.fs h
    in
    let deltas = List.init 151 ~f:(fun i -> Sim.of_ps (from +. (2. *. Float.of_int i))) in
    (* each delta twice: D rising then D falling *)
    let results = run_flop ~d_slew ~clk_slew (List.concat_map deltas ~f:(fun d -> [ d; d ])) in
    List.iter [ true; false ] ~f:(fun rise ->
      let lo, hi = window ~rise in
      let mine = List.filter results ~f:(fun (_, r, _, _, _, _) -> Bool.equal r rise) in
      let xs = List.filter_map mine ~f:(fun (d, _, m, _, _, _) -> Option.map m ~f:(fun _ -> d)) in
      (* outside the aperture: captured iff D changed before it *)
      let mismatches =
        List.count mine ~f:(fun (d, _, m, q, _, _) ->
          let expect_x = d > lo && d < hi in
          let new_v = if rise then Sim.Value.V1 else V0 in
          let old_v = if rise then Sim.Value.V0 else V1 in
          if expect_x
          then Option.is_none m
          else Option.is_some m || not (Sim.Value.equal q (if d <= lo then new_v else old_v)))
      in
      printf
        "  D %s: oracle aperture (%.3f, %.3f) ps; sim metastable for delta in [%.0f, %.0f] ps; mismatches %d of %d\n"
        (if rise then "rising " else "falling")
        (Sim.to_ps lo)
        (Sim.to_ps hi)
        (Sim.to_ps (List.hd_exn xs))
        (Sim.to_ps (List.last_exn xs))
        mismatches
        (List.length mine))
  in
  sweep ~d_slew:input_slew ~clk_slew:input_slew ~from:(-200.);
  (* slow clock: positive hold, negative setup for falling D (the aperture
     lies after the clock edge, so the capture decision is deferred) *)
  sweep ~d_slew:0.02 ~clk_slew:1.0 ~from:(-100.);
  [%expect {|
    D slew 0.05 ns, CLK slew 0.05 ns:
      D rising : oracle aperture (-118.062, -63.872) ps; sim metastable for delta in [-118, -64] ps; mismatches 0 of 151
      D falling: oracle aperture (-115.778, -37.653) ps; sim metastable for delta in [-114, -38] ps; mismatches 0 of 151
    D slew 0.02 ns, CLK slew 1.00 ns:
      D rising : oracle aperture (-15.570, 15.084) ps; sim metastable for delta in [-14, 14] ps; mismatches 0 of 151
      D falling: oracle aperture (63.633, 111.348) ps; sim metastable for delta in [64, 110] ps; mismatches 0 of 151
    |}]
;;

let%expect_test "metastability with positive hold: time never runs backwards" =
  (* Slow clock: a falling D lands in the aperture after the edge (about
     64-111 ps), so the capture decision is deferred to edge + hold. A
     resolution drawn shorter than that must not be scheduled in the past
     ([push] raises if it is). Even cycles load a clean 1 first. *)
  let n = 400 in
  let deltas = List.init n ~f:(fun k -> if k % 2 = 0 then Sim.of_ps (-500.) else Sim.of_ps 90.) in
  let results = run_flop ~d_slew:0.02 ~clk_slew:1.0 deltas in
  let metas = List.filter_map results ~f:(fun (_, _, m, _, _, _) -> m) in
  let early =
    List.count metas ~f:(fun m -> m.resolve_time < m.edge_time + Sim.of_ps 90.)
  in
  let backwards =
    List.count results ~f:(fun (_, _, _, _, _, qs) ->
      let times = List.map qs ~f:fst in
      not (List.equal Int.equal times (List.sort times ~compare:Int.compare)))
  in
  printf "metastable captures: %d of %d; resolved before the D transition: %d; Q times out of order: %d\n"
    (List.length metas) (n / 2) early backwards;
  [%expect {| metastable captures: 200 of 200; resolved before the D transition: 0; Q times out of order: 0 |}]
;;

let%expect_test "metastability: resolution times exponential with tau, values fair" =
  let ol = force olib in
  let n = 4000 in
  (* D edges 85 ps before the clock edge: inside both apertures *)
  let results = run_flop (List.init n ~f:(fun _ -> Sim.of_ps (-85.))) in
  let tau = Sim.Config.default.tau in
  let dts =
    List.filter_map results ~f:(fun (_, _, m, _, _, _) ->
      Option.map m ~f:(fun m -> Sim.to_ns (m.resolve_time - m.edge_time)))
  in
  let k = List.length dts in
  let kf = Float.of_int k in
  let mean = List.sum (module Float) dts ~f:Fn.id /. kf in
  let frac p = Float.of_int (List.count dts ~f:p) /. kf in
  let ones =
    Float.of_int
      (List.count results ~f:(fun (_, _, m, _, _, _) ->
         match m with
         | Some { resolved_to = V1; _ } -> true
         | _ -> false))
    /. kf
  in
  printf "metastable captures: %d of %d\n" k n;
  let check name ~got ~expect ~sd =
    printf "%-22s %.4f  expected %.4f +- %.4f (3 sd): %b\n" name got expect (3. *. sd)
      Float.(abs (got -. expect) < 3. *. sd)
  in
  check "mean resolution (ns)" ~got:mean ~expect:tau ~sd:(tau /. Float.sqrt kf);
  let p1 = Float.exp (-1.) and p2 = Float.exp (-2.) in
  check "P(t > tau)" ~got:(frac (fun d -> Float.(d > tau))) ~expect:p1
    ~sd:(Float.sqrt (p1 *. (1. -. p1) /. kf));
  check "P(t > 2 tau)" ~got:(frac (fun d -> Float.(d > 2. *. tau))) ~expect:p2
    ~sd:(Float.sqrt (p2 *. (1. -. p2) /. kf));
  check "P(resolved to 1)" ~got:ones ~expect:0.5 ~sd:(Float.sqrt (0.25 /. kf));
  (* On Q: X from edge + min(clk->q rise, fall) until edge + t_res + clk->q
     of the resolved value; tables at clock slew [input_slew], load = wire. *)
  let arc = Oracle.timing ol ~cell:"sg13cmos5l_dfrbpq_1" ~out:"Q" ~related:"CLK" () in
  let cq rise = Oracle.fs (fst (Oracle.arc arc ~rise ~slew:input_slew ~load:wire_cap)) in
  let bad =
    List.count results ~f:(fun (_, _, m, _, e, qs) ->
      match m, qs with
      | Some m, [ (tx, X); (tr, v) ] ->
        let r = Sim.Value.equal v V1 in
        abs (tx - (e + Int.min (cq true) (cq false))) > 1
        || abs (tr - (m.resolve_time + cq r)) > 1
      | Some _, _ -> true
      | None, _ -> false)
  in
  printf "Q waveform (X at clk->q, resolved value at t_res + clk->q) mismatches: %d\n" bad;
  [%expect {|
    metastable captures: 4000 of 4000
    mean resolution (ns)   0.0296  expected 0.0300 +- 0.0014 (3 sd): true
    P(t > tau)             0.3548  expected 0.3679 +- 0.0229 (3 sd): true
    P(t > 2 tau)           0.1305  expected 0.1353 +- 0.0162 (3 sd): true
    P(resolved to 1)       0.4850  expected 0.5000 +- 0.0237 (3 sd): true
    Q waveform (X at clk->q, resolved value at t_res + clk->q) mismatches: 0
    |}]
;;

let%expect_test "inertial vs transport: pulses narrower than the gate delay" =
  let tl = force tlib
  and ol = force olib in
  let cell = "sg13cmos5l_dlygate4sd3_1" in
  let circuit =
    Circuit.create_exn
      ~name:"pulse"
      [ Signal.output "y" (Cell.buffer tl.library ~cell ~instance:"dly" (Signal.input "a" 1)) ]
  in
  let arc = Oracle.timing ol ~cell ~out:"X" ~related:"A" () in
  let dr = Oracle.fs (fst (Oracle.arc arc ~rise:true ~slew:input_slew ~load:wire_cap)) in
  let df = Oracle.fs (fst (Oracle.arc arc ~rise:false ~slew:input_slew ~load:wire_cap)) in
  printf "oracle: rise delay %.3f ps, fall delay %.3f ps\n" (Sim.to_ps dr) (Sim.to_ps df);
  let output_pulse pulse_mode w =
    let sim = Sim.create ~config:{ Sim.Config.default with pulse_mode } tl circuit in
    Sim.set_input sim "a" V0 ~at:0;
    let t0 = Sim.of_ns 5. in
    Sim.set_input sim "a" V1 ~at:t0;
    Sim.set_input sim "a" V0 ~at:(t0 + w);
    let ch = ref [] in
    Sim.on_change sim "y" (fun ~time v -> if time > Sim.of_ns 1. then ch := (time, v) :: !ch);
    Sim.run_until sim (Sim.of_ns 20.);
    match List.rev !ch with
    | [] -> None
    | [ (t1, V1); (t2, V0) ] -> Some (t2 - t1)
    | _ -> Some (-1)
  in
  let show = function
    | None -> "swallowed"
    | Some w -> Printf.sprintf "%.3f ps" (Sim.to_ps w)
  in
  let all_ok = ref true in
  List.iter [ 0.3; 0.6; 0.9; 1.1; 1.5 ] ~f:(fun k ->
    let w = Float.iround_nearest_exn (k *. Float.of_int dr) in
    (* oracle: inertial cancels the pending rise if the input falls before it
       lands; transport keeps both edges if the fall lands after the rise *)
    let exp_inertial = if w > dr then Some (w + df - dr) else None in
    let exp_transport = if w + df > dr then Some (w + df - dr) else None in
    let si = output_pulse Inertial w
    and st = output_pulse Transport w in
    if not ([%equal: int option] si exp_inertial && [%equal: int option] st exp_transport)
    then all_ok := false;
    printf "input %7.3f ps (%.1f x rise delay): inertial %-12s transport %s\n" (Sim.to_ps w) k
      (show exp_inertial) (show exp_transport));
  printf "sim equals oracle in both modes: %b\n" !all_ok;
  [%expect {|
    oracle: rise delay 361.402 ps, fall delay 376.299 ps
    input 108.421 ps (0.3 x rise delay): inertial swallowed    transport 123.318 ps
    input 216.841 ps (0.6 x rise delay): inertial swallowed    transport 231.738 ps
    input 325.262 ps (0.9 x rise delay): inertial swallowed    transport 340.159 ps
    input 397.542 ps (1.1 x rise delay): inertial 412.439 ps   transport 412.439 ps
    input 542.103 ps (1.5 x rise delay): inertial 557.000 ps   transport 557.000 ps
    sim equals oracle in both modes: true
    |}]
;;

let%expect_test "counter clocked by a ring node counts ring periods" =
  let tl = force tlib
  and ol = force olib in
  let en = Signal.input "en" 1
  and clr = Signal.input "clr" 1
  and rst = Signal.input "rst" 1
  and cnt_en = Signal.input "cnt_en" 1 in
  let nodes = build_ring tl.library fast_ring ~en in
  let ring_out = nodes.(Array.length nodes - 1) in
  let count =
    Signal.reg_fb
      ~enable:cnt_en
      (Signal.Reg_spec.create ~clock:ring_out ~reset:rst ~clear:clr ())
      ~width:16
      ~f:(fun q -> Signal.(q +:. 1))
  in
  let circuit = Circuit.create_exn ~name:"ring_counter" [ Signal.output "count" count ] in
  let sim = Sim.create tl circuit in
  List.iter [ "en", Sim.Value.V0; "clr", V1; "rst", V0; "cnt_en", V1 ] ~f:(fun (n, v) ->
    Sim.set_input sim n v ~at:0);
  Sim.set_input sim "en" V1 ~at:(Sim.of_ns 10.);
  let t_start = Sim.of_ns 20.
  and t_stop = Sim.of_ns 2020. in
  Sim.set_input sim "clr" V0 ~at:t_start;
  Sim.set_input sim "cnt_en" V0 ~at:t_stop;
  let edges = ref 0 in
  Sim.on_change sim "ring_s8/Y" (fun ~time v ->
    if Sim.Value.equal v V1 && time > t_start && time <= t_stop then Int.incr edges);
  Sim.run_until sim (Sim.of_ns 2100.);
  let period, _ = ring_oracle ol fast_ring in
  printf "oracle: %.1f ps period -> %.2f periods in 2 us\n" (Sim.to_ps period)
    (Float.of_int (t_stop - t_start) /. Float.of_int period);
  printf "counter = observed rising edges: %b; |count - 2 us / period| < 1: %b\n"
    ([%equal: int option] (Sim.to_int sim "count") (Some !edges))
    (let span = t_stop - t_start in
     Float.(abs (of_int !edges -. (of_int span /. of_int period)) < 1.));
  Sim.set_input sim "rst" V1 ~at:(Sim.of_ns 2100.);
  Sim.run_until sim (Sim.of_ns 2101.);
  printf "after async reset: %s\n"
    (Option.value_map (Sim.to_int sim "count") ~default:"X" ~f:Int.to_string);
  [%expect {|
    oracle: 550.6 ps period -> 3632.59 periods in 2 us
    counter = observed rising edges: true; |count - 2 us / period| < 1: true
    after async reset: 0
    |}]
;;

let%expect_test "latch: transparent, holds, D->Q and GATE->Q delays from the tables" =
  let tl = force tlib
  and ol = force olib in
  let cell = "sg13cmos5l_dlhq_1" in
  let q =
    Cell.instantiate tl.library ~cell ~instance:"lat" [ "D", Signal.input "d" 1; "GATE", Signal.input "g" 1 ] "Q"
  in
  let sim = Sim.create tl (Circuit.create_exn ~name:"latch" [ Signal.output "q" q ]) in
  let ns = Sim.of_ns in
  let changes = ref [] in
  Sim.on_change sim "q" (fun ~time v -> changes := (time, v) :: !changes);
  List.iter
    [ 0., "g", Sim.Value.V0; 0., "d", V0; 1., "g", V1; 3., "d", V1; 5., "g", V0; 7., "d", V0; 9., "g", V1 ]
    ~f:(fun (t, n, v) -> Sim.set_input sim n v ~at:(ns t));
  Sim.run_until sim (ns 12.);
  let delay ~related ~rise =
    let arc = Oracle.timing ol ~cell ~out:"Q" ~related () in
    Oracle.fs (fst (Oracle.arc arc ~rise ~slew:input_slew ~load:wire_cap))
  in
  let expected =
    [ ns 1. + delay ~related:"GATE" ~rise:false, Sim.Value.V0 (* opens with D = 0 *)
    ; ns 3. + delay ~related:"D" ~rise:true, V1 (* transparent *)
    ; ns 9. + delay ~related:"GATE" ~rise:false, V0 (* held 1 while closed, reopens on D = 0 *)
    ]
  in
  List.iter expected ~f:(fun (t, v) -> printf "oracle %10.3f ps -> %c\n" (Sim.to_ps t) (Sim.Value.to_char v));
  printf "sim matches: %b\n" ([%equal: (int * Sim.Value.t) list] (List.rev !changes) expected);
  [%expect {|
    oracle   1174.393 ps -> 0
    oracle   3190.012 ps -> 1
    oracle   9174.393 ps -> 0
    sim matches: true
    |}]
;;

let%expect_test "per-instance variation: seeded, applied per stage" =
  let tl = force tlib
  and ol = force olib in
  let circuit = ring_circuit tl.library fast_ring in
  let run seed =
    let config = { Sim.Config.default with variation_sigma = 0.1; seed } in
    let sim = Sim.create ~config tl circuit in
    Sim.set_input sim "en" V0 ~at:0;
    Sim.set_input sim "en" V1 ~at:(Sim.of_ns 5.);
    let p, _ =
      Option.value_exn (measure_period sim "ring_s8/Y" ~from:(Sim.of_ns 20.) ~until:(Sim.of_ns 520.))
    in
    let factors = Sim.instance_factors sim in
    let factor i =
      List.Assoc.find_exn factors (Printf.sprintf "ring_s%d" i) ~equal:String.equal
    in
    let expected, _ = ring_oracle ol fast_ring ~factors:factor in
    p, expected, List.map factors ~f:snd
  in
  let p1, e1, f1 = run 1
  and p1', _, _ = run 1
  and p2, e2, _ = run 2 in
  printf "seed 1: factors %s\n" (String.concat ~sep:" " (List.map f1 ~f:(Printf.sprintf "%.3f")));
  printf "seed 1 period %.3f ps, seed 2 %.3f ps (oracle with the drawn factors)\n" (Sim.to_ps e1) (Sim.to_ps e2);
  printf "sim matches oracle: %b; same seed reproduces: %b\n"
    Float.(abs ((of_int p1 /. of_int e1) -. 1.) < 1e-5 && abs ((of_int p2 /. of_int e2) -. 1.) < 1e-5)
    (p1 = p1');
  [%expect {|
    seed 1: factors 1.130 1.016 1.026 0.929 1.060 1.029 1.009 0.939 1.042
    seed 1 period 568.414 ps, seed 2 537.598 ps (oracle with the drawn factors)
    sim matches oracle: true; same seed reproduces: true
    |}]
;;

(* Reference semantics in plain Hardcaml logic, written from the cell names
   (not from the liberty functions). *)
let reference_logic : (string * string list * ((string -> Signal.t) -> Signal.t)) list =
  let open Signal in
  [ "sg13cmos5l_inv_1", [ "A" ], (fun v -> ~:(v "A"))
  ; "sg13cmos5l_buf_1", [ "A" ], (fun v -> v "A")
  ; "sg13cmos5l_nand2_1", [ "A"; "B" ], (fun v -> ~:(v "A" &: v "B"))
  ; "sg13cmos5l_nor2_1", [ "A"; "B" ], (fun v -> ~:(v "A" |: v "B"))
  ; "sg13cmos5l_o21ai_1", [ "A1"; "A2"; "B1" ], (fun v -> ~:((v "A1" |: v "A2") &: v "B1"))
  ; "sg13cmos5l_a21oi_1", [ "A1"; "A2"; "B1" ], (fun v -> ~:((v "A1" &: v "A2") |: v "B1"))
  ; "sg13cmos5l_xor2_1", [ "A"; "B" ], (fun v -> v "A" ^: v "B")
  ; "sg13cmos5l_mux2_1", [ "A0"; "A1"; "S" ], (fun v -> mux2 (v "S") (v "A1") (v "A0"))
  ; "sg13cmos5l_dlygate4sd1_1", [ "A" ], (fun v -> v "A")
  ]
;;

(* Hardcaml primitives mixed into the cell netlists (built identically in
   both versions; Event_sim gives them zero delay). *)
let hardcaml_ops : (int * (Signal.t list -> Signal.t)) list =
  let open Signal in
  let args3 f = function
    | [ a; b; c ] -> f a b c
    | _ -> assert false
  in
  [ 3, args3 (fun a b _ -> a &: b)
  ; 3, args3 (fun a b _ -> a |: b)
  ; 3, args3 (fun a b _ -> ~:(a ^: b))
  ; 3, args3 (fun a b c -> select (concat_msb [ a; b; c ] +:. 3) ~high:2 ~low:2)
  ; 3, args3 (fun a b c -> concat_msb [ a; b ] <: concat_msb [ c; a ])
  ; 3, args3 (fun a b c -> concat_msb [ a; c ] ==: concat_msb [ b; c ])
  ; 3, args3 (fun a b c -> mux (concat_msb [ a; b ]) [ c; ~:c; a; b ])
  ; 3, args3 (fun a b c -> select (concat_msb [ a; b ] *: concat_msb [ b; c ]) ~high:1 ~low:1)
  ]
;;

let%expect_test "random combinational netlists settle to the Cyclesim reference" =
  let tl = force tlib in
  let rng = Random.State.make [| 42 |] in
  let n_inputs = 5
  and n_cells = 30
  and n_outputs = 6
  and n_vectors = 40 in
  let results = Hashtbl.create (module String) in
  for _netlist = 0 to 9 do
    let ins = List.init n_inputs ~f:(fun i -> Signal.input (Printf.sprintf "i%d" i) 1) in
    (* parallel construction: (cells version, reference version) *)
    let nodes = ref (List.map ins ~f:(fun s -> s, s)) in
    for c = 0 to n_cells - 1 do
      if Random.State.int rng 4 = 0
      then (
        let arity, f = List.nth_exn hardcaml_ops (Random.State.int rng (List.length hardcaml_ops)) in
        let pool = Array.of_list !nodes in
        let picks = List.init arity ~f:(fun _ -> pool.(Random.State.int rng (Array.length pool))) in
        nodes := (f (List.map picks ~f:fst), f (List.map picks ~f:snd)) :: !nodes)
      else (
        let cell, pins, f =
          List.nth_exn reference_logic (Random.State.int rng (List.length reference_logic))
        in
        let pool = Array.of_list !nodes in
        let picks = List.map pins ~f:(fun p -> p, pool.(Random.State.int rng (Array.length pool))) in
        let out = (Cell_library.Cell.outputs (Cell_library.find_exn tl.library cell) |> List.hd_exn).name in
        let a =
          Cell.instantiate tl.library ~cell ~instance:(Printf.sprintf "u%d" c)
            (List.map picks ~f:(fun (p, (a, _)) -> p, a)) out
        in
        let b = f (fun p -> snd (List.Assoc.find_exn picks p ~equal:String.equal)) in
        nodes := (a, b) :: !nodes)
    done;
    let outs = List.take !nodes n_outputs in
    let mk sel = List.mapi outs ~f:(fun i p -> Signal.output (Printf.sprintf "o%d" i) (sel p)) in
    let ca = Circuit.create_exn ~name:"cells" (mk fst) in
    let cb = Circuit.create_exn ~name:"reference" (mk snd) in
    let cyc = Cyclesim.create cb in
    let vectors = List.init n_vectors ~f:(fun _ -> Random.State.int rng (1 lsl n_inputs)) in
    let reference =
      List.map vectors ~f:(fun v ->
        List.iter (Cyclesim.inputs cyc) ~f:(fun (name, p) ->
          let i = Int.of_string (String.chop_prefix_exn name ~prefix:"i") in
          p := Bits.of_int_trunc ~width:1 ((v lsr i) land 1));
        Cyclesim.cycle cyc;
        List.init n_outputs ~f:(fun i ->
          Bits.to_int_trunc !(Cyclesim.out_port cyc (Printf.sprintf "o%d" i))))
    in
    List.iter
      [ "zero delay", Sim.Config.Zero, Sim.Config.Inertial
      ; "liberty, inertial", Liberty, Inertial
      ; "liberty, transport", Liberty, Transport
      ]
      ~f:(fun (name, delay_mode, pulse_mode) ->
        let sim = Sim.create ~config:{ Sim.Config.default with delay_mode; pulse_mode } tl ca in
        let step = Sim.of_ns 20. in
        let bad =
          List.foldi (List.zip_exn vectors reference) ~init:0 ~f:(fun k acc (v, expect) ->
            let at = (k + 1) * step in
            List.iteri ins ~f:(fun i _ ->
              let name = Printf.sprintf "i%d" i in
              if Sim.has_net sim name
              then Sim.set_input sim name (Sim.Value.of_bool ((v lsr i) land 1 = 1)) ~at);
            Sim.run_until sim (at + step - 1);
            let got =
              List.init n_outputs ~f:(fun i ->
                match Sim.value sim (Printf.sprintf "o%d" i) with
                | V0 -> 0
                | V1 -> 1
                | X -> 2)
            in
            acc + List.count (List.zip_exn got expect) ~f:(fun (g, e) -> g <> e))
        in
        Hashtbl.update results name ~f:(fun o -> Option.value o ~default:0 + bad))
  done;
  List.iter (Hashtbl.to_alist results |> List.sort ~compare:[%compare: string * int])
    ~f:(fun (key, data) ->
    printf "%-20s %d mismatching outputs (10 netlists x 40 vectors x 6 outputs)\n" key data);
  [%expect {|
    liberty, inertial    0 mismatching outputs (10 netlists x 40 vectors x 6 outputs)
    liberty, transport   0 mismatching outputs (10 netlists x 40 vectors x 6 outputs)
    zero delay           0 mismatching outputs (10 netlists x 40 vectors x 6 outputs)
    |}]
;;

let%expect_test "VCD" =
  let tl = force tlib in
  let a = Signal.input "a" 1 in
  let y =
    Cell.instantiate tl.library ~cell:"sg13cmos5l_inv_1" ~instance:"i0" [ "A", a ] "Y"
    |> fun x -> Cell.instantiate tl.library ~cell:"sg13cmos5l_inv_1" ~instance:"i1" [ "A", x ] "Y"
  in
  let sim = Sim.create tl (Circuit.create_exn ~name:"vcd" [ Signal.output "y" y ]) in
  Sim.start_tracing sim;
  Sim.set_input sim "a" V0 ~at:0;
  Sim.set_input sim "a" V1 ~at:(Sim.of_ns 1.);
  Sim.run_until sim (Sim.of_ns 2.);
  Sim.write_vcd sim Stdio.stdout;
  [%expect {|
    $timescale 1fs $end
    $scope module event_sim $end
    $var wire 1 ! y $end
    $var wire 1 " i0/Y $end
    $var wire 1 # a $end
    $upscope $end
    $enddefinitions $end
    #0
    $dumpvars
    x!
    x"
    x#
    $end
    #0
    0#
    #37753
    1"
    #60606
    0!
    #1000000
    1#
    #1035880
    0"
    #1057911
    1!
    |}]
;;

let%expect_test "unsupported constructs are rejected" =
  let tl = force tlib in
  let try_ c =
    match Sim.create tl c with
    | _ -> print_endline "accepted"
    | exception Sim.Unsupported s -> print_s s
  in
  let a = Signal.input "a" 1 in
  let inst =
    Instantiation.create () ~name:"my_block" ~instance:"blk" ~inputs:[ "a", a ] ~outputs:[ "y", 1 ]
  in
  try_ (Circuit.create_exn ~name:"hier" [ Signal.output "y" (Instantiation.output inst "y") ]);
  let clock = Signal.input "clock" 1 in
  let mem =
    Signal.multiport_memory
      4
      ~write_ports:
        [| { write_clock = clock; write_address = Signal.zero 2; write_enable = a; write_data = a } |]
      ~read_addresses:[| Signal.zero 2 |]
  in
  try_ (Circuit.create_exn ~name:"mem" [ Signal.output "y" mem.(0) ]);
  [%expect {|
    ("instantiation is not a cell of the timing library (hierarchical instantiations are not supported)"
     (name my_block) (instance blk))
    ("memories are not supported by Event_sim" (signal <unnamed>))
    |}]
;;

let%expect_test "latch: metastable only when D moves inside the closing-edge aperture" =
  let tl = force tlib
  and ol = force olib in
  let cell = "sg13cmos5l_dlhq_1" in
  let c = Oracle.constraint_group ol ~cell ~pin:"D" ~related:"GATE" in
  let window ~rise =
    let kind = if rise then "rise_constraint" else "fall_constraint" in
    let s = Oracle.interp (Oracle.tbl (c ~timing_type:"setup_falling") kind) input_slew input_slew in
    let h = Oracle.interp (Oracle.tbl (c ~timing_type:"hold_falling") kind) input_slew input_slew in
    Oracle.fs (-.s), Oracle.fs h
  in
  let lo, hi = window ~rise:true in
  printf "oracle aperture for rising D: (%.3f, %.3f) ps around GATE falling\n" (Sim.to_ps lo) (Sim.to_ps hi);
  let try_ delta =
    let q =
      Cell.instantiate tl.library ~cell ~instance:"lat" [ "D", Signal.input "d" 1; "GATE", Signal.input "g" 1 ] "Q"
    in
    let sim = Sim.create tl (Circuit.create_exn ~name:"latch" [ Signal.output "q" q ]) in
    let close = Sim.of_ns 5. in
    Sim.set_input sim "g" V1 ~at:0;
    Sim.set_input sim "d" V0 ~at:0;
    Sim.set_input sim "g" V0 ~at:close;
    Sim.set_input sim "d" V1 ~at:(close + delta);
    Sim.run_until sim (Sim.of_ns 10.);
    printf "D rises at %+8.3f ps: metastable %b, Q %c\n" (Sim.to_ps delta)
      (not (List.is_empty (Sim.metastable_events sim)))
      (Sim.Value.to_char (Sim.value sim "q"))
  in
  try_ (lo - Sim.of_ps 20.);
  try_ ((lo + hi) / 2);
  try_ (hi + Sim.of_ps 20.);
  [%expect {|
    oracle aperture for rising D: (-108.759, -100.478) ps around GATE falling
    D rises at -128.759 ps: metastable false, Q 1
    D rises at -104.618 ps: metastable true, Q 1
    D rises at  -80.478 ps: metastable false, Q 0
    |}]
;;

let%expect_test "a ring in zero-delay mode is reported, not hung" =
  let tl = force tlib in
  let sim =
    Sim.create ~config:{ Sim.Config.default with delay_mode = Zero } tl (ring_circuit tl.library fast_ring)
  in
  Sim.set_input sim "en" V0 ~at:0;
  Sim.set_input sim "en" V1 ~at:(Sim.of_ns 1.);
  (match Sim.run_until sim (Sim.of_ns 2.) with
   | () -> print_endline "returned"
   | exception e -> print_s (Exn.sexp_of_t e));
  [%expect {|
    ("Event_sim: more than a million events at one instant (a loop with zero delay?)"
     (time 1000000))
    |}]
;;
