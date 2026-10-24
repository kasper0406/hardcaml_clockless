(** A free-running ring oscillator with ring-clocked period counters: a fine
    time reference that never runs out of range.

    {v
         enable --+
                  NAND --> node0 --> INV --> node1 --> INV --> ... --> node(L-1)
            fb ---+          |                                     |
            ^                +--> counter A (rising)  counter B (falling)
            +---- length-select mux <--- taps node(n-1), n in lengths
    v}

    The loop is a NAND enable gate plus an even number of inverters, so a
    selected length [n] (odd) has [n] inverting stages. With uniform stage
    delay [d], one transition circulates and the period is [2 n d]. The ring
    {e time} [T], in stage delays, is [k * 2n + p], where the {e phase} [p] in
    [0, 2n) is where the wavefront is, and [k] counts periods.

    {2 State}

    Node [i] satisfies [node i (t) = node 0 (t - i d) xor (i land 1)], and node
    0 is high for [p] in [0, n) and low for [p] in [n, 2n). So [u i = node i
    xor (i land 1)] is, over positions [0..n-1], a run of [p+1] ones followed
    by zeros when [p < n], or a run of [p-n+1] zeros followed by ones when [p
    >= n]. The pattern continues around the loop inverted (position [n] would
    be [not u 0]), so the decoder filters every position, ends included, with
    a majority of itself and its two neighbours. A single misread position
    then moves the decoded phase by at most one stage, modulo [2n].

    {2 Counters}

    Counter A counts rising edges of node 0, at [p = 0]. Counter B counts
    falling edges, at [p = n]. Each is a Johnson prescaler of [johnson_bits]
    flops (single-bit change, one gate level, fast enough for the ring
    clock), plus a Gray counter enabled on the Johnson wrap. Both are clocked
    by the same edge, so a counter is only unreliable for a short {e settle}
    window after its own edge. That window is roughly clock-to-Q plus the
    clock buffer, and is about the same number of stage delays at every corner,
    because both scale with the process.

    A sampler uses counter A when [p] is in [[s, n + s)] and counter B
    otherwise, where [s = (n + settle + 1) / 2]. Then neither is read within
    [settle] stages after its edge, with equal margin before its next edge.
    From A, [k = A]. From B, [k = B - (p >= n ? 1 : 0)], because B also counts
    the fall in the current period once [p >= n].

    {2 Loop length and loads}

    Every stage should carry the same load, or its bin is wider. With flop
    cells, the counter clocks are therefore buffered off node 0: two inverters
    for counter A, one for counter B. A length-select mux inside the loop makes
    one very wide bin (~3x a stage per mux level). Event_sim measured bins up to
    ~400 ps and twice the calibrated error compared with a fixed length (see
    test/test_ring_timing.ml). Prefer a single length, and escape injection
    locking to the system clock by changing the clock frequency.

    {2 Reset}

    While [enable] is low the ring is static (node 0 high, nodes alternate),
    and both counters are held at 0 by an asynchronous reset. Releasing
    [enable] races the first ring edge, so a counter may start at 0 or 1.
    Calibration uses differences, so this is harmless, but it is not
    hidden. *)

open! Base
open Hardcaml
open Signal

module type Config = sig
  (** Selectable loop lengths: odd, ascending, at most 4. The longest sets the
      number of physical stages. *)
  val lengths : int list

  (** Johnson prescaler flops K: it counts 2K ring periods. A power of two. *)
  val johnson_bits : int

  (** Gray counter width, counting prescaler wraps. *)
  val gray_width : int

  (** Stage delays after a counter's clock edge during which it may be read
      half-updated. *)
  val counter_settle_stages : int
end

(** Johnson + Gray period counter. Its encoding is shared by the hardware
    counter, the behavioural model and the decoder. *)
module Counter = struct
  let width ~johnson_bits ~gray_width = johnson_bits + gray_width

  (* Johnson state index m in [0, 2K): m <= K has the low m bits set; m > K has
     the top 2K - m bits set. *)
  let johnson_of_index ~johnson_bits m =
    let k = johnson_bits in
    if m <= k then (1 lsl m) - 1 else ((1 lsl k) - 1) land lnot ((1 lsl (m - k)) - 1)
  ;;

  let gray_of_int g = g lxor (g lsr 1)

  (** Encode a period count as [johnson @ gray] bits (Gray in the high bits). *)
  let encode_int ~johnson_bits ~gray_width c =
    let m = c % (2 * johnson_bits) in
    let g = c / (2 * johnson_bits) % (1 lsl gray_width) in
    (gray_of_int g lsl johnson_bits) lor johnson_of_index ~johnson_bits m
  ;;

  (** Hardware, with [flop name d] building one flip-flop per bit. *)
  let create_with ~johnson_bits ~gray_width ~flop =
    let k = johnson_bits in
    let j = wire k in
    let g = wire gray_width in
    let wrap = j ==:. 1 lsl (k - 1) in
    let j_next = if k = 1 then ~:j else sel_bottom j ~width:(k - 1) @: ~:(msb j) in
    let flops prefix d =
      concat_lsb (List.mapi (bits_lsb d) ~f:(fun i b -> flop (Printf.sprintf "%s%d" prefix i) b))
    in
    Signal.assign j (flops "j" j_next);
    Signal.assign g (flops "g" (mux2 wrap (gray_increment g ~by:1) g));
    g @: j
  ;;

  (** Hardware: a counter of Hardcaml registers clocked by [spec]. *)
  let create ~johnson_bits ~gray_width spec =
    create_with ~johnson_bits ~gray_width ~flop:(fun _ d -> reg spec d)
  ;;

  (** Decode [johnson @ gray] bits to the period count. *)
  let decode ~johnson_bits ~gray_width bits =
    let k = johnson_bits in
    let j = sel_bottom bits ~width:k in
    let g = sel_top bits ~width:gray_width in
    let ones = uresize (popcount j) ~width:(num_bits_to_represent (2 * k)) in
    let m = mux2 (msb j) (of_int_trunc ~width:(Signal.width ones) (2 * k) -: ones) ones in
    let count_width = gray_width + num_bits_to_represent ((2 * k) - 1) in
    let g_bin = uresize (gray_to_binary g) ~width:count_width in
    (g_bin *: of_int_trunc ~width:(num_bits_to_represent (2 * k)) (2 * k)
     |> uresize ~width:count_width)
    +: uresize m ~width:count_width
  ;;
end

(** An ideal ring, in plain OCaml: a unit-delay gate simulation. It is the
    reference behaviour that the behavioural model is driven from and that
    tests compare the decoder against. It is deliberately not written in terms
    of the closed form above. *)
module Ideal = struct
  type t =
    { n : int
    ; nodes : bool array (** all physical nodes *)
    ; mutable time : int (** stage delays since [enable] rose *)
    ; mutable count_a : int (** rising edges of node 0 *)
    ; mutable count_b : int (** falling edges of node 0 *)
    ; mutable since_a : int (** stage delays since counter A's last edge *)
    ; mutable since_b : int
    }

  (** The static state while disabled: node 0 high (NAND with enable low),
      then alternating. Enable rises at time 0. *)
  let create ~n ~stages =
    if n < 3 || n % 2 = 0 || n > stages then raise_s [%message "Ideal: bad length" (n : int)];
    { n
    ; nodes = Array.init stages ~f:(fun i -> i % 2 = 0)
    ; time = 0
    ; count_a = 0
    ; count_b = 0
    ; since_a = Int.max_value / 2
    ; since_b = Int.max_value / 2
    }
  ;;

  (** Advance one stage delay: every gate output becomes its function of the
      inputs one delay ago. *)
  let step t =
    let prev = Array.copy t.nodes in
    let fb = prev.(t.n - 1) in
    t.nodes.(0) <- not (true && fb);
    for i = 1 to Array.length t.nodes - 1 do
      t.nodes.(i) <- not prev.(i - 1)
    done;
    t.time <- t.time + 1;
    t.since_a <- t.since_a + 1;
    t.since_b <- t.since_b + 1;
    if (not prev.(0)) && t.nodes.(0)
    then (
      t.count_a <- t.count_a + 1;
      t.since_a <- 0);
    if prev.(0) && not t.nodes.(0)
    then (
      t.count_b <- t.count_b + 1;
      t.since_b <- 0)
  ;;

  (** Counter values as a sampler would read them. A counter read fewer than
      [settle] stages after its edge returns the old or new value, chosen by
      [pick_old]. *)
  let counts ?(settle = 0) ?(pick_old = fun () -> false) t =
    let read c since = if since < settle && pick_old () then c - 1 else c in
    read t.count_a t.since_a, read t.count_b t.since_b
  ;;
end

module type S = sig
  include Config

  val max_length : int
  val select_width : int
  val counter_width : int (** encoded Johnson + Gray bits *)
  val count_width : int (** decoded period count *)
  val phase_width : int (** bits for [0, 2 * max_length) *)
  val time_width : int (** [count * 2n + phase] *)

  (** [s] for each length, in stage delays (see the module doc). *)
  val select_threshold : int -> int

  (** The ring's observable state. *)
  module State : sig
    type 'a t =
      { nodes : 'a (** [max_length] bits, bit i = node i *)
      ; count_a : 'a (** encoded counter A *)
      ; count_b : 'a (** encoded counter B *)
      }
    [@@deriving hardcaml]
  end

  module Model_stimulus : sig
    type 'a t =
      { nodes : 'a (** node values at this clock edge, from {!Ideal} *)
      ; count_a : 'a (** period count A as an integer *)
      ; count_b : 'a
      }
    [@@deriving hardcaml]
  end

  (** A flip-flop cell with an active-low asynchronous reset. *)
  type flop =
    { cell : string
    ; clk : string
    ; d : string
    ; reset_n : string
    ; q : string
    }

  type cells =
    { library : Cell_library.t
    ; nand2 : string
    ; inverter : string
    ; mux2 : string
    ; flop : flop option
    (** Counter flip-flops as hand-placed cells (named [<name>_cnt_{a,b}_{j,g}<i>];
        counter B is clocked through an inverter [<name>_clkinv_b]). [None]:
        Hardcaml registers. *)
    }

  type implementation =
    | Cells of cells (** hand-placed cells, for Verilog generation *)
    | Model of Signal.t Model_stimulus.t (** behavioural, for Cyclesim *)

  (** A pin to break timing loops at ([set_disable_timing]). *)
  type loop_break =
    { instance : string
    ; pin : string
    }
  [@@deriving sexp_of]

  (** Build the ring and its counters. Cell instances are [<name>_nand],
      [<name>_inv<i>] and [<name>_mux<j>]; nodes are named [<name>_node<i>]. *)
  module Analog : sig
    val create
      :  name:string
      -> implementation
      -> enable:Signal.t
      -> length_sel:Signal.t
      -> Signal.t State.t * loop_break list
  end

  (** The decoded time of a settled ring state (see the module doc). *)
  module Decoded : sig
    type 'a t =
      { phase : 'a (** [p] in [0, 2n) *)
      ; periods : 'a (** [k] *)
      ; time : 'a (** [k * 2n + p], modulo [2 ^ time_width] *)
      ; pattern_error : 'a (** the node pattern was not a single wavefront *)
      }
    [@@deriving hardcaml]
  end

  (** Combinational decode of a ring state that has already been captured and
      settled (e.g. synchronised to a clock, or held in a capture register):
      majority filter, phase, counter selection by phase, time. [length_sel]
      selects the loop length as in {!Analog.create}. Shared by
      {!Clk_sampler} and [Pin_capture]. *)
  val decode : length_sel:Signal.t -> Signal.t State.t -> Signal.t Decoded.t

  (** The system-clock sampler: registers the state on [clock] (first stage
      named [<name>_sample_*] with dont_touch/keep), then decodes. *)
  module Clk_sampler : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; enable : 'a
        ; length_sel : 'a
        ; state : 'a State.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { valid : 'a (** the ring was enabled for this sample *)
        ; phase : 'a (** [p] in [0, 2n) *)
        ; periods : 'a (** [k] *)
        ; time : 'a (** [k * 2n + p], modulo [2 ^ time_width] *)
        ; pattern_error : 'a (** the node pattern was not a single wavefront *)
        }
      [@@deriving hardcaml]
    end

    (** [first_stage]: build the asynchronous first stage from flip-flop cells
        (instances named [<name>_sample_*], their outputs [<name>_sample_*_q])
        instead of kept Hardcaml registers. *)
    val create
      :  ?first_stage:Cell_library.t * flop
      -> name:string
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end

module Make (C : Config) : S = struct
  include C

  let () =
    let bad msg = raise_s [%message "Ring: bad configuration" msg (lengths : int list)] in
    if List.is_empty lengths || List.length lengths > 4 then bad "1 to 4 lengths";
    if not (List.is_sorted_strictly lengths ~compare:Int.compare) then bad "lengths ascending";
    List.iter lengths ~f:(fun n -> if n < 3 || n % 2 = 0 then bad "lengths odd and >= 3");
    if johnson_bits < 1 || gray_width < 1 then bad "counter widths >= 1";
    (* The decoded count wraps modulo 2^count_width; the counter wraps modulo
       2 * johnson_bits * 2^gray_width. They agree (so [B - 1] at count 0 is
       right) only when [johnson_bits] is a power of two. *)
    if not (Int.is_pow2 johnson_bits) then bad "johnson_bits a power of two";
    List.iter lengths ~f:(fun n ->
      let s = (n + counter_settle_stages + 1) / 2 in
      if counter_settle_stages < 0 || s <= counter_settle_stages || s >= n
      then bad "counter_settle_stages too large for the shortest length")
  ;;

  let max_length = List.last_exn lengths
  let select_width = Int.max 1 (num_bits_to_represent (List.length lengths - 1))
  let counter_width = Counter.width ~johnson_bits ~gray_width
  let count_width = gray_width + num_bits_to_represent ((2 * johnson_bits) - 1)
  let phase_width = num_bits_to_represent ((2 * max_length) - 1)
  let time_width = count_width + phase_width
  let select_threshold n = (n + counter_settle_stages + 1) / 2

  module State = struct
    type 'a t =
      { nodes : 'a [@bits max_length]
      ; count_a : 'a [@bits counter_width]
      ; count_b : 'a [@bits counter_width]
      }
    [@@deriving hardcaml]
  end

  module Model_stimulus = struct
    type 'a t =
      { nodes : 'a [@bits max_length]
      ; count_a : 'a [@bits count_width]
      ; count_b : 'a [@bits count_width]
      }
    [@@deriving hardcaml]
  end

  type flop =
    { cell : string
    ; clk : string
    ; d : string
    ; reset_n : string
    ; q : string
    }

  type cells =
    { library : Cell_library.t
    ; nand2 : string
    ; inverter : string
    ; mux2 : string
    ; flop : flop option
    }

  type implementation =
    | Cells of cells
    | Model of Signal.t Model_stimulus.t

  let flop_cell library (f : flop) ~instance ~clk ~reset_n d =
    Cell.instantiate library ~cell:f.cell ~instance [ f.clk, clk; f.d, d; f.reset_n, reset_n ] f.q
  ;;

  type loop_break =
    { instance : string
    ; pin : string
    }
  [@@deriving sexp_of]

  (* Identify cell pins from their functions, so any library works. *)
  module Pins = struct
    let inputs lib cell =
      List.map (Cell_library.Cell.inputs (Cell_library.find_exn lib cell)) ~f:(fun p -> p.name)
    ;;

    let output_and_function lib cell =
      match Cell_library.Cell.outputs (Cell_library.find_exn lib cell) with
      | [ { name; function_ = Some f; _ } ] -> name, f
      | _ -> raise_s [%message "Ring: expected one output with a function" (cell : string)]
    ;;

    let check lib cell ~arity ~f:reference ~what =
      let ins = inputs lib cell in
      let out, fn = output_and_function lib cell in
      if List.length ins <> arity then raise_s [%message "Ring: wrong pin count" (cell : string) what];
      List.iter (List.init (1 lsl arity) ~f:Fn.id) ~f:(fun bits ->
        let env p =
          let i, _ = List.findi_exn ins ~f:(fun _ q -> String.equal p q) in
          (bits lsr i) land 1 = 1
        in
        if Bool.( <> ) (Bool_expr.eval fn ~env) (reference (List.map ins ~f:env))
        then raise_s [%message "Ring: cell does not implement" (cell : string) what]);
      ins, out
    ;;

    let inverter lib cell =
      match check lib cell ~arity:1 ~what:"an inverter" ~f:(function [ a ] -> not a | _ -> false) with
      | [ a ], y -> a, y
      | _ -> assert false
    ;;

    let nand2 lib cell =
      match
        check lib cell ~arity:2 ~what:"a nand2" ~f:(function [ a; b ] -> not (a && b) | _ -> false)
      with
      | [ a; b ], y -> a, b, y
      | _ -> assert false
    ;;

    (* Returns (select, input when select=0, input when select=1, output). *)
    let mux2 lib cell =
      let ins = inputs lib cell in
      let out, fn = output_and_function lib cell in
      let eval assign = Bool_expr.eval fn ~env:(fun p -> List.Assoc.find_exn assign p ~equal:String.equal) in
      let candidates =
        List.filter_map ins ~f:(fun s ->
          match List.filter ins ~f:(Fn.non (String.equal s)) with
          | [ x; y ] ->
            let all = List.init 8 ~f:Fn.id in
            let ok d0 d1 =
              List.for_all all ~f:(fun b ->
                let vs = (b land 1 = 1), (b land 2 = 2), (b land 4 = 4) in
                let sv, xv, yv = vs in
                let assign = [ s, sv; x, xv; y, yv ] in
                Bool.equal (eval assign) (if sv then List.Assoc.find_exn assign d1 ~equal:String.equal
                                          else List.Assoc.find_exn assign d0 ~equal:String.equal))
            in
            if ok x y then Some (s, x, y) else if ok y x then Some (s, y, x) else None
          | _ -> None)
      in
      match candidates with
      | [ (s, d0, d1) ] -> s, d0, d1, out
      | _ -> raise_s [%message "Ring: cell is not a 2:1 mux" (cell : string)]
    ;;
  end

  module Analog = struct
    let create ~name implementation ~enable ~length_sel =
      if Signal.width enable <> 1 then raise_s [%message "Ring: enable must be 1 bit"];
      if Signal.width length_sel <> select_width
      then raise_s [%message "Ring: length_sel width" (select_width : int)];
      let nodes_named nodes =
        concat_lsb (List.mapi nodes ~f:(fun i s -> s -- Printf.sprintf "%s_node%d" name i))
      in
      match implementation with
      | Model m ->
        Model_stimulus.Of_signal.assert_widths m;
        let encode c =
          (* The hardware counter's encoding of every count value, as a table.
             Simulation-only, so its size does not matter. *)
          let table =
            List.init (1 lsl count_width) ~f:(fun v ->
              of_int_trunc ~width:counter_width (Counter.encode_int ~johnson_bits ~gray_width v))
          in
          mux c table
        in
        ( { State.nodes = nodes_named (bits_lsb m.nodes)
          ; count_a = encode m.count_a
          ; count_b = encode m.count_b
          }
        , [] )
      | Cells { library; nand2; inverter; mux2; flop } ->
        let nand_a, nand_b, nand_y = Pins.nand2 library nand2 in
        let inv_a, inv_y = Pins.inverter library inverter in
        let mux_s, mux_d0, mux_d1, mux_x = Pins.mux2 library mux2 in
        let fb = wire 1 in
        let nand_instance = name ^ "_nand" in
        let node0 =
          Cell.instantiate library ~cell:nand2 ~instance:nand_instance [ nand_a, enable; nand_b, fb ]
            nand_y
        in
        let nodes = Array.create ~len:max_length node0 in
        for i = 1 to max_length - 1 do
          nodes.(i)
          <- Cell.instantiate
               library
               ~cell:inverter
               ~instance:(Printf.sprintf "%s_inv%d" name i)
               [ inv_a, nodes.(i - 1) ]
               inv_y
        done;
        let taps = List.map lengths ~f:(fun n -> nodes.(n - 1)) in
        let mux_count = ref 0 in
        let mux_cell s d0 d1 =
          let instance = Printf.sprintf "%s_mux%d" name !mux_count in
          Int.incr mux_count;
          Cell.instantiate library ~cell:mux2 ~instance [ mux_s, s; mux_d0, d0; mux_d1, d1 ] mux_x
        in
        let feedback =
          match taps with
          | [ t ] -> t
          | [ t0; t1 ] -> mux_cell length_sel.:(0) t0 t1
          | [ t0; t1; t2 ] ->
            mux_cell length_sel.:(1) (mux_cell length_sel.:(0) t0 t1) t2
          | [ t0; t1; t2; t3 ] ->
            mux_cell length_sel.:(1) (mux_cell length_sel.:(0) t0 t1) (mux_cell length_sel.:(0) t2 t3)
          | _ -> assert false
        in
        Signal.assign fb feedback;
        let count_a, count_b =
          match flop with
          | None ->
            let reset = ~:enable in
            let spec edge = Reg_spec.create ~clock:node0 ~clock_edge:edge ~reset ~reset_level:High () in
            ( Counter.create ~johnson_bits ~gray_width (spec Rising)
            , Counter.create ~johnson_bits ~gray_width (spec Falling) )
          | Some f ->
            (* Rising-edge cells; counter B is clocked by node 0 inverted. *)
            (* Counter clocks are buffered so node 0 carries only small,
               uniform loads like every other node. *)
            let inv instance x = Cell.instantiate library ~cell:inverter ~instance [ inv_a, x ] inv_y in
            let node0_n = inv (name ^ "_clkinv_b") node0 in
            let node0_buf = inv (name ^ "_clkinv_a1") node0 |> inv (name ^ "_clkinv_a2") in
            let counter tag clk =
              Counter.create_with ~johnson_bits ~gray_width ~flop:(fun bit d ->
                flop_cell library f ~instance:(Printf.sprintf "%s_cnt_%s_%s" name tag bit) ~clk
                  ~reset_n:enable d)
            in
            counter "a" node0_buf, counter "b" node0_n
        in
        ( { State.nodes = nodes_named (Array.to_list nodes)
          ; count_a = count_a -- (name ^ "_count_a")
          ; count_b = count_b -- (name ^ "_count_b")
          }
        , [ { instance = nand_instance; pin = nand_b } ] )
    ;;
  end

  module Decoded = struct
    type 'a t =
      { phase : 'a [@bits phase_width]
      ; periods : 'a [@bits count_width]
      ; time : 'a [@bits time_width]
      ; pattern_error : 'a
      }
    [@@deriving hardcaml]
  end

  let decode ~length_sel (s : Signal.t State.t) : Signal.t Decoded.t =
    if Signal.width length_sel <> select_width
    then raise_s [%message "Ring.decode: length_sel width" (select_width : int)];
    State.Of_signal.assert_widths s;
    (* Per selected length n: phase, error, periods. Results muxed by length. *)
    let per_length n =
      let odd = concat_lsb (List.init max_length ~f:(fun j -> if j % 2 = 1 then vdd else gnd)) in
      let u = s.nodes ^: odd in
      (* The pattern continues around the loop inverted: virtual position
         -1 is [not u (n-1)] and virtual position n is [not u 0]. So every
         position, ends included, is filtered with its true neighbours. *)
      let ub j = if j < 0 then ~:(u.:(n - 1)) else if j >= n then ~:(u.:(0)) else u.:(j) in
      let maj a b c = a &: b |: (a &: c) |: (b &: c) in
      let f = List.init n ~f:(fun j -> maj (ub (j - 1)) (ub j) (ub (j + 1))) in
      let u0 = List.hd_exn f in
      let same = concat_lsb (List.map f ~f:(fun b -> ~:(b ^: u0))) in
      let run = trailing_ones same in
      let pw = phase_width in
      let phase =
        uresize run ~width:pw -:. 1
        +: mux2 u0 (zero pw) (of_int_trunc ~width:pw n)
      in
      (* Pattern check: at most one change along 0..n-1 after filtering. *)
      let changes =
        List.init (n - 1) ~f:(fun j -> List.nth_exn f j ^: List.nth_exn f (j + 1))
        |> concat_lsb |> popcount
      in
      let pattern_error = changes >:. 1 in
      let thr = select_threshold n in
      let use_a = phase >=:. thr &: (phase <:. n + thr) in
      let a = Counter.decode ~johnson_bits ~gray_width s.count_a in
      let b = Counter.decode ~johnson_bits ~gray_width s.count_b in
      let periods = mux2 use_a a (b -: uresize (phase >=:. n) ~width:count_width) in
      let time =
        (uresize periods ~width:time_width *: of_int_trunc ~width:phase_width (2 * n)
         |> uresize ~width:time_width)
        +: uresize phase ~width:time_width
      in
      phase, periods, time, pattern_error
    in
    let results = List.map lengths ~f:per_length in
    let pick f =
      let xs = List.map results ~f in
      mux length_sel (xs @ List.init ((1 lsl select_width) - List.length xs) ~f:(fun _ -> List.last_exn xs))
    in
    { Decoded.phase = pick (fun (p, _, _, _) -> p)
    ; periods = pick (fun (_, k, _, _) -> k)
    ; time = pick (fun (_, _, t, _) -> t)
    ; pattern_error = pick (fun (_, _, _, e) -> e)
    }
  ;;

  module Clk_sampler = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; enable : 'a
        ; length_sel : 'a [@bits select_width]
        ; state : 'a State.t [@rtlprefix "state_"]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { valid : 'a
        ; phase : 'a [@bits phase_width]
        ; periods : 'a [@bits count_width]
        ; time : 'a [@bits time_width]
        ; pattern_error : 'a
        }
      [@@deriving hardcaml]
    end

    let create ?first_stage ~name scope (i : Signal.t I.t) : Signal.t O.t =
      I.Of_signal.assert_widths i;
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create ~clock:i.clock () in
      (* First stage: catches the asynchronous ring state. Kept and named. *)
      let first_bit b nm =
        match first_stage with
        | None ->
          let r = reg spec b in
          List.iter Cell.attributes ~f:(fun a -> ignore (add_attribute r a : Signal.t));
          r -- nm
        | Some (library, f) ->
          (* the instance keeps [nm]; a wire of the same name would rename it *)
          flop_cell library f ~instance:nm ~clk:i.clock ~reset_n:vdd b -- (nm ^ "_q")
      in
      let first x nm =
        let nm = Printf.sprintf "%s_sample_%s" name nm in
        if Signal.width x = 1
        then first_bit x nm
        else concat_lsb (List.mapi (bits_lsb x) ~f:(fun k b -> first_bit b (Printf.sprintf "%s_%d" nm k)))
      in
      let s1 =
        { State.nodes =
            concat_lsb
              (List.mapi (bits_lsb i.state.nodes) ~f:(fun k b -> first b (Printf.sprintf "node%d" k)))
        ; count_a = first i.state.count_a "count_a"
        ; count_b = first i.state.count_b "count_b"
        }
      in
      (* Second stage: resolves metastability. *)
      let s = State.map s1 ~f:(reg spec) in
      let length_sel = reg spec (reg spec i.length_sel) in
      let enabled = reg spec (reg spec (i.enable &: ~:(i.clear))) in
      let d = decode ~length_sel s in
      let r x = reg spec x in
      { O.valid = r enabled -- "valid"
      ; phase = r d.phase
      ; periods = r d.periods
      ; time = r d.time
      ; pattern_error = r (d.pattern_error &: enabled)
      }
    ;;
  end
end
