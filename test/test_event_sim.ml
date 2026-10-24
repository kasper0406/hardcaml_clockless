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
