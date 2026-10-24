(** XOR-merged, ring-referenced input edge capture.

    Every edge on any enabled input pin is captured with the ring
    oscillator's time ({!Ring}) and a snapshot of all pin levels, in an
    asynchronous front end clocked by the pins themselves, and drained in
    arrival order by the system clock.

    {v
     pins --+--> NAND(pin, en) --> XOR tree --> merged --+--> buf  --> leaves --> bank R (rising)
            |    (en from drain)   (hand-placed)         +--> inv  --> leaves --> bank F (falling)
            +--> buf --------------------------------------------> slot D muxes (pin snapshot)
            +--> edge counter: toggle on rise, toggle on fall (2-bit Gray), clocked by the pin
     ring nodes, counters A/B --> buf --> slot D muxes

     bank: Johnson write pointer + [slots] records {nodes, count_a, count_b, pins},
           each bit a flop whose D is mux(selected, new, Q): the clock is never gated.

     drain (clk): 2-flop syncs of pointers, edge counters, levels, merged level
           -> read the next bank in turn (rising and falling toggles alternate)
           -> attribution = snapshot xor previous snapshot
           -> ring decode (Ring.decode) -> checks -> event
    v}

    {2 Front}

    - {b Merge.} Pin [i] is gated by [NAND(pin_i, en_i)]; the XOR tree of the
      gated pins toggles once per edge of an enabled pin. (The NAND inverts,
      which only changes the merged level by a constant.)
    - {b Capture.} Bank R is clocked by the merged signal's rising edge,
      bank F by its falling edge, through hand-placed buffer trees. Toggles
      alternate between the banks, so reading R, F, R, ... preserves arrival
      order. Each bank has a Johnson write pointer ([ptr_bits] flops, a
      single bit changes per write, so it synchronises safely and a runt
      clock leaves it either advanced or not, never invalid) and [slots]
      record slots. A slot loads when the pointer selects it: [D = sel ? new
      : Q].
    - {b Snapshot race.} The record's pin bits must show a pin's {e new}
      level when its own edge clocks the bank: the pin -> buffer -> mux -> D
      path must beat pin -> NAND -> XOR tree -> clock tree -> CLK by the
      setup time. The difference is also the {e collision window}: an edge on
      another pin that arrives within it is in the same snapshot, so both
      pins are attributed to one event (the second toggle then captures an
      unchanged snapshot, an {e empty} event, which is dropped and counted).
      Edges closer than about one XOR gate delay cancel in the tree and are
      not captured at all; the level check reports them (below).
    - {b Edge counters.} Per pin, a flop toggled on each rising edge and one
      toggled on each falling edge form a 2-bit Gray count of edges, clocked
      by the pin itself. Both reset to 0 whatever the pin level, so the pair
      steps through the Gray code forwards from a low pin and backwards from
      a high one; the drain decodes it relative to the synchronised level.

    {2 Drain}

    - Write pointers, edge counters, pin levels and the merged level are
      synchronised by two flops each, sampled on the same clock edge.
    - The next record is read from the bank whose turn it is when that bank
      is non-empty. Attribution: [changed = (snapshot xor previous) & enabled
      & ~guard]. An event with no changed pin is {e empty}: dropped, counted.
      Several changed pins = near-coincident edges reported as one event.
    - The ring time is decoded from the record ({!Ring.S.decode}).
    - Events leave the third pipeline stage combinationally, 2 cycles after
      the read (6-7 cycles after the edge), so the overwrite check below can
      be applied to the event itself. Raw slot bits are readable through [raw_select] / [raw].

    {2 Integrity checks (everything is flagged, nothing silently wrong)}

    - {b Edge counts / level check.} In a cycle where the FIFO is empty and a
      pin's synchronised edge counter and level did not change since the
      previous cycle, every edge counted has been captured and drained (the
      pointer moves within ~1 ns of the counter). Then the events attributed
      to the pin (mod 4) must equal its edge count, and its level must equal
      the last snapshot. A level mismatch (edges that cancelled in the XOR,
      or were too narrow to clock the bank) is reported as an {e untimed}
      event on that pin and counted; a count mismatch (lost or phantom edges)
      sets [lost_edge]. Both resynchronise. A pin that toggles every cycle
      without a pause is not checked until it pauses.
    - {b Runts.} A merged pulse that one bank's clock tree passes and the
      other's swallows gives one bank an extra write. When the other bank
      stays exactly one record ahead for 3 cycles the drain reads it next
      (re-aligning the order) and counts a [runt].
    - {b Pointers.} An invalid Johnson code, or banks whose fill differs by
      more than alternation allows, sets the sticky [capture_fault], counts,
      marks events in the pipeline, and resynchronises: the drain discards
      what is unread, waits until pointers and levels are stable for two
      cycles, and restarts from there (the next bank from the merged level).
      A pointer that stays invalid is reset ([ptr_reset_n], auto-reset).
    - {b Overflow.} A bank with more than [slots] unread records, or a write
      pointer that has passed the slot of an event that was being read
      (checked when the event leaves the pipeline), sets [overflow], marks
      the events in the pipeline and resynchronises. The pointer range is
      [2 * ptr_bits], so more than that many writes to one bank between two
      clock samples cannot be seen by the pointers; the edge counters then
      report the loss (unless a pin lost a multiple of 4 edges).
    - {b Stale attributions.} Each record also holds every pin delayed by
      a chain of delay cells ([delay], longer than the pin -> capture clock
      delay). A pin attributed to an event must differ from its delayed copy
      (it changed within the delay before the capture); otherwise its change
      is older, its own capture was lost, and it is reported as [stale]
      (level known, no time) instead of [changed].
    - {b Capture balance.} Every capture read is one merged toggle. An event
      attributing k pins is followed by k - 1 empty captures (the other
      pins' toggles). Edges that cancelled in the XOR tree have no capture,
      so a later capture attributes them: once all is drained and stable,
      captures minus attributions is then negative, and the pins attributed
      since it was last 0 get [merge_error].
    - {b Order.} Event times must not go backwards by more than
      [order_tolerance] stages ([order_error]).
    - {b Enable changes.} Changing a pin's enable toggles its NAND output
      when the pin is high. The pin is then {e guarded}: changes on it are
      reported in [reconfig], not as edges, until [guard_cycles] have passed
      and the FIFO has drained; its checks then resynchronise. A disabled
      pin's changes are ignored. So enabling and disabling never produce
      phantom edges, but edges on that pin during the guard are reported only
      as [reconfig].

    {2 Model}

    [Model] replaces the front with registers updated once per clock cycle
    from a stimulus that lists the captures of that cycle, for Cyclesim
    ({!Reference} computes such stimuli from timed edges). *)

open! Base
open Hardcaml
open Signal

module type Config = sig
  (** Number of input pins, 1 to 16. *)
  val pins : int

  (** Record slots per bank (rising, falling), 2 to 4. *)
  val slots : int

  (** Cycles a pin stays guarded after its enable changes (>= 3). *)
  val guard_cycles : int

  (** Stage delays an event time may go backwards before [order_error]. *)
  val order_tolerance : int

  (** Width of the status counters. *)
  val status_width : int

  (** [Model] only: most captures in one clock cycle. *)
  val model_captures : int

  (** Most flops per hand-placed clock leaf buffer. *)
  val clock_leaf_fanout : int
end

(** Johnson write pointers: [bits] flops, [2 * bits] states. Slot = index mod
    [slots]. *)
module Pointer = struct
  let bits ~slots = if slots = 2 then 3 else slots
  let range ~slots = 2 * bits ~slots
  let index_width ~slots = num_bits_to_represent (range ~slots - 1)

  let code_of_index ~slots m =
    Ring.Counter.johnson_of_index ~johnson_bits:(bits ~slots) (m % range ~slots)
  ;;

  let valid_code ~slots c =
    let j = bits ~slots in
    List.count (List.init (j - 1) ~f:Fn.id) ~f:(fun k ->
      (c lsr k) land 1 <> (c lsr (k + 1)) land 1)
    <= 1
  ;;

  let index_of_code ~slots c =
    let j = bits ~slots in
    let ones = Int.popcount c in
    if (c lsr (j - 1)) land 1 = 1 then (2 * j) - ones else ones
  ;;

  (* Hardware *)
  let to_index ~slots s =
    let j = bits ~slots in
    let w = index_width ~slots in
    let ones = uresize (popcount s) ~width:w in
    mux2 (msb s) (of_int_trunc ~width:w (2 * j) -: ones) ones
  ;;

  let valid ~slots s =
    let j = bits ~slots in
    List.init (j - 1) ~f:(fun k -> s.:(k) ^: s.:(k + 1)) |> concat_lsb |> popcount <=:. 1
  ;;

  let next s = sel_bottom s ~width:(width s - 1) @: ~:(msb s)

  let slot_of_index ~slots idx =
    let sw = Int.max 1 (num_bits_to_represent (slots - 1)) in
    match slots with
    | 2 -> idx.:(0)
    | 4 -> sel_bottom idx ~width:2
    | 3 -> uresize (mux2 (idx >=:. 3) (idx -:. 3) idx) ~width:sw
    | _ -> raise_s [%message "Pin_capture: slots must be 2..4" (slots : int)]
  ;;

  (* [(a - b) mod range], width [index_width + 1]. *)
  let sub_mod ~slots a b =
    let w = index_width ~slots + 1 in
    let a = uresize a ~width:w
    and b = uresize b ~width:w in
    mux2 (a <: b) (a -: b +:. range ~slots) (a -: b)
  ;;

  let inc_mod ~slots r = mux2 (r ==:. range ~slots - 1) (zero (width r)) (r +:. 1)
end


(** Capture semantics in plain OCaml, independent of the hardware: which
    merged toggles happen, what each capture's snapshot holds, and which
    events the drain should report. Times are integers in any unit (stage
    delays for {!Make.Model_driver}). It is the oracle for the Cyclesim tests
    and generates [Model] stimuli. *)
module Reference = struct
  (** A toggle of [pin] at [time]. *)
  type edge =
    { time : int
    ; pin : int
    }
  [@@deriving sexp_of, compare]

  type capture =
    { time : int (** the merged toggle *)
    ; snapshot : int (** all pin levels, bit i = pin i *)
    ; delayed : int (** pin levels [recent] before the toggle *)
    }
  [@@deriving sexp_of, compare]

  type event =
    { time : int
    ; snapshot : int
    ; changed : int
    ; stale : int (** changed, but not within [recent] before the capture *)
    }
  [@@deriving sexp_of, compare]

  let sort_edges edges = List.stable_sort edges ~compare:(fun (a : edge) b -> Int.compare a.time b.time)

  (** Pin levels just after [time] (edges at [time] included). *)
  let levels_at ~initial edges time =
    List.fold edges ~init:initial ~f:(fun acc (e : edge) ->
      if e.time <= time then acc lxor (1 lsl e.pin) else acc)
  ;;

  (** Front semantics.
      - The merged signal toggles on each edge of a pin enabled at that time,
        and when a pin's enable changes while the pin is high
        ([enable_changes]: (time, new mask), applied after edges at the same
        time).
      - Two consecutive toggles less than [swallow] apart cancel: neither is
        captured.
      - A capture at [t] snapshots the pin levels at [t + merge_window], and
        their delayed copies, the levels at [t - recent] (default [2 *
        merge_window]).
      Returns the captures and the cancelled toggles' times. *)
  let captures ?(enable_changes = []) ?recent ~initial ~enabled ~merge_window ~swallow edges =
    let recent = Option.value recent ~default:(2 * merge_window) in
    let edges = sort_edges edges in
    let enable_at t =
      List.fold enable_changes ~init:enabled ~f:(fun acc (time, m) -> if time < t then m else acc)
    in
    let from_pins =
      List.filter_map edges ~f:(fun e ->
        if enable_at e.time land (1 lsl e.pin) <> 0 then Some e.time else None)
    in
    let from_enables =
      List.concat_map enable_changes ~f:(fun (time, m) ->
        let before = enable_at time in
        let lv = levels_at ~initial edges time in
        List.filter_map (List.init 63 ~f:Fn.id) ~f:(fun i ->
          if (before lxor m) land lv land (1 lsl i) <> 0 then Some time else None))
    in
    let toggles = List.sort (from_pins @ from_enables) ~compare:Int.compare in
    let rec cancel kept lost = function
      | a :: b :: rest when b - a < swallow -> cancel kept (b :: a :: lost) rest
      | a :: rest -> cancel (a :: kept) lost rest
      | [] -> List.rev kept, List.rev lost
    in
    let kept, lost = cancel [] [] toggles in
    ( List.map kept ~f:(fun time ->
        { time
        ; snapshot = levels_at ~initial edges (time + merge_window)
        ; delayed = levels_at ~initial edges (time - recent)
        })
    , lost )
  ;;

  (** Drain semantics with a constant enable mask: one event per capture whose
      snapshot differs from the previous one on an enabled pin. A changed pin
      whose delayed copy already shows the new level changed long before
      this capture (its own toggle was lost): it is [stale], not timed. *)
  let events ~initial ~enabled (captures : capture list) =
    List.folding_map captures ~init:initial ~f:(fun prev (c : capture) ->
      let diff = (c.snapshot lxor prev) land enabled in
      let stale = diff land lnot (c.snapshot lxor c.delayed) in
      let changed = diff land lnot stale in
      ( c.snapshot
      , if diff = 0 then None else Some { time = c.time; snapshot = c.snapshot; changed; stale } ))
    |> List.filter_opt
  ;;
end

module Make (C : Config) (R : Ring.S) = struct
  include C

  let () =
    let bad msg = raise_s [%message "Pin_capture: bad configuration" (msg : string)] in
    if pins < 1 || pins > 16 then bad "pins in 1..16";
    if slots < 2 || slots > 4 then bad "slots in 2..4";
    if guard_cycles < 3 then bad "guard_cycles >= 3";
    if status_width < 1 then bad "status_width >= 1";
    if model_captures < 1 then bad "model_captures >= 1";
    if clock_leaf_fanout < 1 then bad "clock_leaf_fanout >= 1"
  ;;

  let ptr_bits = Pointer.bits ~slots
  let ptr_range = Pointer.range ~slots
  let index_width = Pointer.index_width ~slots
  let slot_width = Int.max 1 (num_bits_to_represent (slots - 1))
  let capture_count_width = num_bits_to_represent model_captures
  let raw_select_width = num_bits_to_represent ((2 * slots) - 1)

  (** One capture: the ring state and the pin levels. *)
  module Record = struct
    type 'a t =
      { nodes : 'a [@bits R.max_length]
      ; count_a : 'a [@bits R.counter_width]
      ; count_b : 'a [@bits R.counter_width]
      ; pins : 'a [@bits pins]
      ; delayed : 'a [@bits pins] (** pins through the recent-edge delay *)
      }
    [@@deriving hardcaml]
  end

  let record_width = Record.sum_of_port_widths

  (** What the drain reads from the asynchronous front. *)
  module Front = struct
    type 'a t =
      { ptr_r : 'a [@bits ptr_bits]
      ; ptr_f : 'a [@bits ptr_bits]
      ; slots_r : 'a Record.t list [@length slots] [@rtlmangle "_"]
      ; slots_f : 'a Record.t list [@length slots] [@rtlmangle "_"]
      ; edges : 'a [@bits 2 * pins] (** pin i: bit 2i toggles on rise, 2i+1 on fall *)
      ; levels : 'a [@bits pins]
      ; merged : 'a
      }
    [@@deriving hardcaml]
  end

  (** Simulation-only stimulus for [Model]: what happened during the clock
      cycle that ends at this edge. *)
  module Model_stimulus = struct
    type 'a t =
      { captures : 'a [@bits capture_count_width]
      (** merged toggles in this cycle, in order; the first goes to bank R
          if the merged level was low *)
      ; capture : 'a Record.t list [@length model_captures] [@rtlmangle "_"]
      ; edges : 'a [@bits 2 * pins] (** edge counter flops at this edge, as [Front.edges] *)
      ; levels : 'a [@bits pins] (** pin levels at this edge *)
      ; corrupt_r : 'a [@bits ptr_bits] (** XOR-ed into the R pointer (fault injection) *)
      ; corrupt_f : 'a [@bits ptr_bits]
      }
    [@@deriving hardcaml]
  end

  type cells =
    { library : Cell_library.t
    ; nand2 : string
    ; xor2 : string
    ; inverter : string
    ; buffer : string
    ; mux2 : string
    ; flop : R.flop (** with active-low asynchronous reset *)
    ; delay : string * int
    (** buffer/delay cell and count for the recent-edge delay of each pin;
        it must exceed the pin -> capture clock delay at every corner *)
    }

  type implementation =
    | Cells of cells
    | Model of Signal.t Model_stimulus.t

  (** The hand-placed front. Instance names, with [n = name]:
      [<n>_gate<i>] (NAND), [<n>_xor<k>], [<n>_clk{r,f}] (trunks),
      [<n>_clk{r,f}_leaf<k>], [<n>_d{node,cnta,cntb,pin}<i>] (data buffers),
      [<n>_{r,f}_ptr<k>] (pointer flops), [<n>_{r,f}_sel<k>[_buf<m>]],
      [<n>_{r,f}_s<slot>_<field><bit>] (slot flops) and [..._mux],
      [<n>_ec{r,f}<i>] (edge counter flops). *)
  module Front_cells = struct
    let create ~name (c : cells) ~pins:pin_bus ~enable ~ptr_reset_n ~edge_reset_n
      ~(ring : Signal.t R.State.t)
      =
      let lib = c.library in
      let n fmt = Printf.ksprintf (fun s -> name ^ "_" ^ s) fmt in
      let inv_a, inv_y = Cell_pins.inverter lib c.inverter in
      let buf_a, buf_x = Cell_pins.buffer lib c.buffer in
      let nand_a, nand_b, nand_y = Cell_pins.nand2 lib c.nand2 in
      let xor_a, xor_b, xor_x = Cell_pins.xor2 lib c.xor2 in
      let mux_s, mux_a0, mux_a1, mux_x = Cell_pins.mux2 lib c.mux2 in
      let inv instance a = Cell.instantiate lib ~cell:c.inverter ~instance [ inv_a, a ] inv_y in
      let buf instance a = Cell.instantiate lib ~cell:c.buffer ~instance [ buf_a, a ] buf_x in
      let xor instance a b =
        Cell.instantiate lib ~cell:c.xor2 ~instance [ xor_a, a; xor_b, b ] xor_x
      in
      let mux instance ~s ~a0 ~a1 =
        Cell.instantiate lib ~cell:c.mux2 ~instance [ mux_s, s; mux_a0, a0; mux_a1, a1 ] mux_x
      in
      let flop instance ~clk ~reset_n d =
        Cell.instantiate
          lib
          ~cell:c.flop.cell
          ~instance
          [ c.flop.clk, clk; c.flop.d, d; c.flop.reset_n, reset_n ]
          c.flop.q
      in
      let pin i = pin_bus.:(i) in
      (* Merge. *)
      let gated =
        List.init pins ~f:(fun i ->
          Cell.instantiate
            lib
            ~cell:c.nand2
            ~instance:(n "gate%d" i)
            [ nand_a, pin i; nand_b, enable.:(i) ]
            nand_y)
      in
      let xors = ref 0 in
      let rec tree = function
        | [ x ] -> x
        | xs ->
          let rec pairs = function
            | a :: b :: rest ->
              let k = !xors in
              Int.incr xors;
              xor (n "xor%d" k) a b :: pairs rest
            | rest -> rest
          in
          tree (pairs xs)
      in
      let merged = tree gated -- n "merged" in
      (* Data buffers: every ring node and counter bit carries one more,
         uniform, load. *)
      let dbuf tag x = concat_lsb (List.mapi (bits_lsb x) ~f:(fun i b -> buf (n "%s%d" tag i) b)) in
      let fresh =
        { Record.nodes = dbuf "dnode" ring.nodes
        ; count_a = dbuf "dcnta" ring.count_a
        ; count_b = dbuf "dcntb" ring.count_b
        ; pins = dbuf "dpin" pin_bus
        ; delayed =
            (let cell, count = c.delay in
             concat_lsb
               (List.init pins ~f:(fun i ->
                  let line = Delay_line.create lib ~name:(n "dly%d" i) ~cell ~length:count (pin i) in
                  buf (n "ddly%d" i) line.taps.(count - 1))))
        }
      in
      let field_names = Record.(to_list (map port_names ~f:Fn.id)) in
      let fresh_bits =
        List.concat
          (List.map2_exn field_names (Record.to_list fresh) ~f:(fun f s ->
             List.mapi (bits_lsb s) ~f:(fun i b -> Printf.sprintf "%s%d" f i, b)))
      in
      let flops_per_bank = (slots * record_width) + ptr_bits in
      let leaves = (flops_per_bank + clock_leaf_fanout - 1) / clock_leaf_fanout in
      let bank tag trunk =
        let leaf = Array.init leaves ~f:(fun k -> buf (n "clk%s_leaf%d" tag k) trunk) in
        let next_leaf =
          let k = ref 0 in
          fun () ->
            let l = leaf.(!k % leaves) in
            Int.incr k;
            l
        in
        (* Pointer. *)
        let q = wire ptr_bits in
        let d =
          List.init ptr_bits ~f:(fun k ->
            if k = 0 then inv (n "%s_ptrinv" tag) (msb q) else q.:(k - 1))
        in
        Signal.assign
          q
          (concat_lsb
             (List.mapi d ~f:(fun k dk ->
                flop (n "%s_ptr%d" tag k) ~clk:(next_leaf ()) ~reset_n:ptr_reset_n dk)));
        (* Slot selects: (net, loads when high). *)
        let selects =
          if slots = 2
          then (
            let p = xor (n "%s_sel_x" tag) (xor (n "%s_sel_p" tag) q.:(0) q.:(1)) q.:(2) in
            [ p, false; p, true ])
          else
            List.init slots ~f:(fun k ->
              if k = 0
              then xor (n "%s_sel0" tag) q.:(0) q.:(ptr_bits - 1), false
              else xor (n "%s_sel%d" tag k) q.:(k) q.:(k - 1), true)
        in
        (* Buffer each select net for its mux fanout. *)
        let sel_buffers =
          let per = clock_leaf_fanout in
          let cache = Hashtbl.create (module Int) in
          List.mapi selects ~f:(fun k (s, high) ->
            let key = if slots = 2 then 0 else k in
            let loads = if slots = 2 then 2 * record_width else record_width in
            let nbuf = (loads + per - 1) / per in
            let bufs =
              Hashtbl.find_or_add cache key ~default:(fun () ->
                Array.init nbuf ~f:(fun m -> buf (n "%s_sel%d_buf%d" tag key m) s))
            in
            bufs, high)
        in
        let slot_records =
          List.mapi sel_buffers ~f:(fun slot (bufs, high) ->
            let count = ref 0 in
            let bits =
              List.map fresh_bits ~f:(fun (bit_name, fresh_bit) ->
                (* Spread the loads over the buffers; with 2 slots both
                   slots share the buffers of the parity net. *)
                let k = if slots = 2 then (slot * record_width) + !count else !count in
                let sel = bufs.(k % Array.length bufs) in
                Int.incr count;
                let qw = wire 1 in
                let inst = n "%s_s%d_%s" tag slot bit_name in
                let a0, a1 = if high then qw, fresh_bit else fresh_bit, qw in
                let dm = mux (inst ^ "_mux") ~s:sel ~a0 ~a1 in
                let qf = flop inst ~clk:(next_leaf ()) ~reset_n:vdd dm in
                Signal.assign qw qf;
                qf)
            in
            Record.Of_signal.unpack (concat_lsb bits))
        in
        q, slot_records
      in
      let trunk_r = buf (n "clkr") merged in
      let trunk_f = inv (n "clkf") merged in
      let ptr_r, slots_r = bank "r" trunk_r in
      let ptr_f, slots_f = bank "f" trunk_f in
      (* Edge counters. *)
      let edges =
        List.init pins ~f:(fun i ->
          let toggle tag clk =
            let qw = wire 1 in
            let q = flop (n "ec%s%d" tag i) ~clk ~reset_n:edge_reset_n (inv (n "ec%s%d_inv" tag i) qw) in
            Signal.assign qw q;
            q
          in
          let r = toggle "r" (pin i) in
          let f = toggle "f" (inv (n "ecclk%d" i) (pin i)) in
          [ r; f ])
        |> List.concat
        |> concat_lsb
      in
      { Front.ptr_r; ptr_f; slots_r; slots_f; edges; levels = pin_bus; merged }
    ;;
  end

  (** Behavioural front for Cyclesim: the captures of each cycle are written
      at its clock edge, in order, alternating between the banks with the
      merged level. *)
  module Front_model = struct
    let create ~clock ~ptr_reset (s : Signal.t Model_stimulus.t) =
      Model_stimulus.Of_signal.assert_widths s;
      let spec = Reg_spec.create ~clock () in
      let m = wire 1 in
      let ptr_r = wire ptr_bits in
      let ptr_f = wire ptr_bits in
      let slots_r = List.init slots ~f:(fun _ -> Record.Of_signal.wires ()) in
      let slots_f = List.init slots ~f:(fun _ -> Record.Of_signal.wires ()) in
      let write ptr records cap en =
        let slot = Pointer.slot_of_index ~slots (Pointer.to_index ~slots ptr) in
        let records =
          List.mapi records ~f:(fun k r -> Record.Of_signal.mux2 (en &: (slot ==:. k)) cap r)
        in
        mux2 en (Pointer.next ptr) ptr, records
      in
      let m', pr, pf, sr, sf =
        List.foldi s.capture ~init:(m, ptr_r, ptr_f, slots_r, slots_f)
          ~f:(fun k (m, pr, pf, sr, sf) cap ->
            let active = s.captures >:. k in
            let rising = ~:m in
            let pr, sr = write pr sr cap (active &: rising) in
            let pf, sf = write pf sf cap (active &: ~:rising) in
            m ^: active, pr, pf, sr, sf)
      in
      Signal.assign m (reg spec m' -- "model_merged");
      let ptr_reg w p c nm =
        Signal.assign w (reg spec (mux2 ptr_reset (zero ptr_bits) (p ^: c)) -- nm)
      in
      ptr_reg ptr_r pr s.corrupt_r "model_ptr_r";
      ptr_reg ptr_f pf s.corrupt_f "model_ptr_f";
      List.iter2_exn slots_r sr ~f:(fun w x -> Record.Of_signal.assign w (Record.Of_signal.reg spec x));
      List.iter2_exn slots_f sf ~f:(fun w x -> Record.Of_signal.assign w (Record.Of_signal.reg spec x));
      { Front.ptr_r; ptr_f; slots_r; slots_f; edges = s.edges; levels = s.levels; merged = m }
    ;;
  end


  (** Drives a [Model] in Cyclesim from timed edges: a {!Ring.Ideal} ring with
      [stages_per_cycle] stage delays per clock cycle supplies each capture's
      ring state, so a capture at stage time [t] decodes to ring time
      [t + n - 1]. Clock edge [k] is at stage time [k * stages_per_cycle]; the
      first call to {!cycle} sets the inputs for edge 1. *)
  module Model_driver = struct
    type t =
      { ring : Ring.Ideal.t
      ; stages_per_cycle : int
      ; mutable edges : Reference.edge list (** pending, sorted *)
      ; mutable captures : Reference.capture list (** pending, sorted *)
      ; mutable cycle : int
      ; mutable levels : int
      ; mutable edge_flops : int (** as the cells: bit 2i toggles on a rise of pin i, 2i+1 on a fall *)
      ; mutable corrupt_r : int
      ; mutable corrupt_f : int
      }

    let create ~n ~stages_per_cycle ~initial ~edges ~captures =
      { ring = Ring.Ideal.create ~n ~stages:R.max_length
      ; stages_per_cycle
      ; edges = Reference.sort_edges edges
      ; captures =
          List.stable_sort captures ~compare:(fun (a : Reference.capture) b ->
            Int.compare a.time b.time)
      ; cycle = 0
      ; levels = initial
      ; edge_flops = 0
      ; corrupt_r = 0
      ; corrupt_f = 0
      }
    ;;

    (** XOR [r] and [f] into the write pointers at the next edge. *)
    let corrupt t ~r ~f =
      t.corrupt_r <- r;
      t.corrupt_f <- f
    ;;

    let now t = t.cycle * t.stages_per_cycle

    let cycle t (s : Bits.t ref Model_stimulus.t) =
      let t1 = (t.cycle + 1) * t.stages_per_cycle in
      let caps, rest = List.split_while t.captures ~f:(fun c -> c.time <= t1) in
      t.captures <- rest;
      if List.length caps > model_captures
      then raise_s [%message "Model_driver: too many captures in one cycle" (t.cycle : int)];
      let records =
        List.map caps ~f:(fun c ->
          while t.ring.time < c.time do
            Ring.Ideal.step t.ring
          done;
          let a, b = Ring.Ideal.counts t.ring in
          let nodes =
            Array.foldi t.ring.nodes ~init:0 ~f:(fun i acc v -> if v then acc lor (1 lsl i) else acc)
          in
          let enc c = Ring.Counter.encode_int ~johnson_bits:R.johnson_bits ~gray_width:R.gray_width c in
          { Record.nodes; count_a = enc a; count_b = enc b; pins = c.snapshot; delayed = c.delayed })
      in
      while t.ring.time < t1 - 1 do
        Ring.Ideal.step t.ring
      done;
      let evs, rest = List.split_while t.edges ~f:(fun e -> e.time <= t1) in
      t.edges <- rest;
      List.iter evs ~f:(fun e ->
        t.levels <- t.levels lxor (1 lsl e.pin);
        let rise = t.levels land (1 lsl e.pin) <> 0 in
        t.edge_flops <- t.edge_flops lxor (1 lsl ((2 * e.pin) + if rise then 0 else 1)));
      let set r w v = r := Bits.of_int_trunc ~width:w v in
      set s.captures capture_count_width (List.length records);
      List.iteri s.capture ~f:(fun k (cap : Bits.t ref Record.t) ->
        let rc = List.nth records k |> Option.value ~default:{ Record.nodes = 0; count_a = 0; count_b = 0; pins = 0; delayed = 0 } in
        Record.iter2 cap (Record.map2 rc Record.port_widths ~f:(fun v w -> v, w)) ~f:(fun r (v, w) -> set r w v));
      set s.edges (2 * pins) t.edge_flops;
      set s.levels pins t.levels;
      set s.corrupt_r ptr_bits t.corrupt_r;
      set s.corrupt_f ptr_bits t.corrupt_f;
      t.corrupt_r <- 0;
      t.corrupt_f <- 0;
      t.cycle <- t.cycle + 1
    ;;
  end

  module Event = struct
    type 'a t =
      { valid : 'a
      ; untimed : 'a (** found by the level check: no time *)
      ; pins : 'a [@bits pins] (** snapshot of all pin levels *)
      ; changed : 'a [@bits pins] (** pins with an edge in this event *)
      ; reconfig : 'a [@bits pins] (** guarded pins whose level changed (not edges) *)
      ; stale : 'a [@bits pins]
      (** pins whose change is older than the recent-edge delay: their own
          capture was lost (e.g. cancelled in the XOR), so no time *)
      ; bank : 'a (** 0: merged rising, 1: falling *)
      ; time : 'a [@bits R.time_width]
      ; phase : 'a [@bits R.phase_width]
      ; periods : 'a [@bits R.count_width]
      ; pattern_error : 'a
      ; order_error : 'a
      ; overflow : 'a (** possibly overwritten, or read just before an overflow *)
      ; fault : 'a (** read just before a capture fault *)
      }
    [@@deriving hardcaml]
  end

  module Status = struct
    type 'a t =
      { resyncing : 'a
      ; capture_fault : 'a (** sticky *)
      ; fault_count : 'a [@bits status_width]
      ; overflow : 'a (** sticky *)
      ; overflow_count : 'a [@bits status_width]
      ; lost_edge : 'a [@bits pins] (** sticky, per pin *)
      ; lost_edge_count : 'a [@bits status_width]
      ; level_mismatch : 'a [@bits pins] (** sticky, per pin *)
      ; level_mismatch_count : 'a [@bits status_width]
      ; merge_error : 'a [@bits pins]
      (** sticky, per pin: attributions not matched by captures (edges that
          cancelled in the XOR were attributed to a later capture) *)
      ; merge_error_count : 'a [@bits status_width]
      ; empty_count : 'a [@bits status_width]
      ; stale_count : 'a [@bits status_width]
      ; runt_count : 'a [@bits status_width]
      (** a bank missed a toggle the other saw (read order re-aligned) *)
      ; fault_now : 'a
      ; runt_now : 'a
      ; overflow_now : 'a
      ; lost_edge_now : 'a [@bits pins]
      ; level_mismatch_now : 'a [@bits pins]
      ; merge_error_now : 'a [@bits pins]
      ; ptr_reset : 'a (** the front's write pointers are being reset *)
      ; enabled : 'a [@bits pins] (** enables applied to the front *)
      ; guarded : 'a [@bits pins]
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; clear_status : 'a (** clears sticky flags and counters *)
      ; pins : 'a [@bits pins] (** asynchronous; unused in [Model] mode *)
      ; pin_enable : 'a [@bits pins]
      ; length_sel : 'a [@bits R.select_width]
      ; raw_select : 'a [@bits raw_select_width]
      (** slot shown on [raw]: R slots, then F slots *)
      ; ring : 'a R.State.t [@rtlmangle "_"] (** unused in [Model] mode *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { event : 'a Event.t [@rtlmangle "_"]
      ; status : 'a Status.t [@rtlmangle "_"]
      ; raw : 'a Record.t [@rtlmangle "_"]
      (** raw bits of the slot [raw_select], straight from the front (not
          synchronised: sample it twice, while that slot is not written) *)
      }
    [@@deriving hardcaml]
  end

  (** The synchronous drain. Returns its outputs and the front controls
      (enables, active-low pointer reset, active-low edge counter reset). *)
  module Drain = struct
    type controls =
      { enable : Signal.t
      ; ptr_reset_n : Signal.t
      ; edge_reset_n : Signal.t
      ; ptr_reset : Signal.t (** active high, for [Model] *)
      }

    (* Per-stage event fields carried through the pipeline. *)
    module Stage = struct
      type 'a t =
        { valid : 'a
        ; untimed : 'a
        ; bank : 'a
        ; index : 'a [@bits index_width]
        ; pins : 'a [@bits pins]
        ; changed : 'a [@bits pins]
        ; reconfig : 'a [@bits pins]
        ; stale : 'a [@bits pins]
        ; overflow : 'a
        ; fault : 'a
        }
      [@@deriving hardcaml]
    end

    let create ?first_stage ~name scope ~clock ~clear ~clear_status ~pin_enable ~length_sel
      ~raw_select (front : Signal.t Front.t)
      =
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create ~clock () in
      (* State stays cleared until the synchronisers (2 stages + history)
         hold values sampled after [clear]. *)
      let clear =
        List.fold (List.init 4 ~f:Fn.id) ~init:[ clear ] ~f:(fun acc _ ->
          reg spec (List.hd_exn acc) :: acc)
        |> List.reduce_exn ~f:( |: )
      in
      let specc = Reg_spec.create ~clock ~clear () in
      let r x = reg spec x in
      let rc ?clear_to x = reg specc ?clear_to x in
      (* Two-flop synchronisers; the first stage is kept and named. *)
      let first_bit b nm =
        match first_stage with
        | None ->
          let q = reg spec b in
          List.iter Cell.attributes ~f:(fun a -> ignore (add_attribute q a : Signal.t));
          q -- nm
        | Some (library, (f : R.flop)) ->
          Cell.instantiate
            library
            ~cell:f.cell
            ~instance:nm
            [ f.clk, clock; f.d, b; f.reset_n, vdd ]
            f.q
          (* the instance keeps [nm]; a wire of the same name would rename it *)
          -- (nm ^ "_q")
      in
      let sync x tag =
        concat_lsb
          (List.mapi (bits_lsb x) ~f:(fun k b ->
             first_bit b (Printf.sprintf "%s_sync_%s%d" name tag k)))
        |> r
      in
      let wr = sync front.ptr_r "ptr_r" -- "ptr_r" in
      let wf = sync front.ptr_f "ptr_f" -- "ptr_f" in
      let ec = sync front.edges "edges" -- "edges" in
      let lv = sync front.levels "levels" -- "levels" in
      let mg = sync front.merged "merged" -- "merged" in
      let wr_p = r wr
      and wf_p = r wf
      and ec_p = r ec
      and lv_p = r lv
      and mg_p = r mg in
      let idx_r = Pointer.to_index ~slots wr in
      let idx_f = Pointer.to_index ~slots wf in
      let valid_r = Pointer.valid ~slots wr in
      let valid_f = Pointer.valid ~slots wf in
      (* Edge counts (mod 4) from the Gray pairs, relative to the level: the
         flops reset to 0 whatever the pin level, so from a high pin the pair
         steps through the Gray code backwards ([fb @: (fb ^: rb)] would count
         down). The high bit is the flop of the edge type not last seen.
         Only used while the pin's level and counters are stable. *)
      let ec_count i =
        let rb = ec.:(2 * i)
        and fb = ec.:((2 * i) + 1) in
        mux2 lv.:(i) fb rb @: (fb ^: rb)
      in
      (* --- State --- *)
      let resync = wire 1 in
      let rd_r = wire index_width in
      let rd_f = wire index_width in
      let next_f = wire 1 in
      let prev_snap = wire pins in
      let cnt = Array.init pins ~f:(fun _ -> wire 2) in
      let need_resync = wire pins in
      let en = wire pins in
      let guard = wire pins in
      let guard_timer = wire (num_bits_to_represent guard_cycles) in
      let alt_wait = wire 2 in
      let ar_cnt = wire 3 in
      let normal = ~:resync in
      let avail_r = Pointer.sub_mod ~slots idx_r rd_r -- "avail_r" in
      let avail_f = Pointer.sub_mod ~slots idx_f rd_f -- "avail_f" in
      ignore (next_f -- "next_f" : Signal.t);
      ignore (prev_snap -- "prev_snap" : Signal.t);
      ignore (resync -- "resync" : Signal.t);
      let a = mux2 next_f avail_f avail_r in
      let b = mux2 next_f avail_r avail_f in
      let pointer_bad = ~:valid_r |: ~:valid_f in
      let alt_transient = b ==: a +:. 1 in
      let alt_ok = a ==: b |: (a ==: b +:. 1) in
      let alt_bad = ~:alt_ok &: ~:alt_transient in
      (* The other bank one ahead for 3 cycles (longer than any synchroniser
         skew): the expected bank missed a runt toggle that the other bank's
         clock tree passed. Read the other bank from now on. *)
      let runt = ~:resync &: alt_transient &: (alt_wait ==:. 3) -- "runt" in
      let fault_now = normal &: (pointer_bad |: alt_bad) -- "fault_now" in
      let ovf_now = normal &: ~:fault_now &: (avail_r >:. slots |: (avail_f >:. slots)) in
      let do_read = normal &: ~:fault_now &: ~:ovf_now &: (a <>:. 0) -- "do_read" in
      let all_empty = avail_r ==:. 0 &: (avail_f ==:. 0) in
      let do_check = normal &: ~:fault_now &: all_empty -- "do_check" in
      (* Read. *)
      let rd = mux2 next_f rd_f rd_r in
      let slot = Pointer.slot_of_index ~slots rd in
      let pick slots_list =
        mux slot (List.map slots_list ~f:Record.Of_signal.pack)
      in
      let record =
        Record.Of_signal.unpack (mux2 next_f (pick front.slots_f) (pick front.slots_r))
      in
      let diff = record.pins ^: prev_snap -- "diff" in
      let stale = diff &: en &: ~:guard &: ~:(record.pins ^: record.delayed) in
      let changed = diff &: en &: ~:guard &: ~:stale in
      let reconfig = diff &: guard in
      (* Checks. *)
      let lv_stable = ~:(lv ^: lv_p) in
      let per_pin =
        List.init pins ~f:(fun i ->
          let stable = lv_stable.:(i) &: (ec.:(2 * i) ==: ec_p.:(2 * i))
                       &: (ec.:((2 * i) + 1) ==: ec_p.:((2 * i) + 1)) in
          let active = do_check &: en.:(i) &: ~:(guard.:(i)) &: stable in
          let resync_i = active &: need_resync.:(i) in
          let check_i = active &: ~:(need_resync.:(i)) in
          let level_bad = check_i &: (lv.:(i) ^: prev_snap.:(i)) in
          let missing = ec_count i -: cnt.(i) in
          let count_bad = check_i &: (missing <>: uresize level_bad ~width:2) in
          resync_i, level_bad, count_bad)
      in
      let resync_pins = concat_lsb (List.map per_pin ~f:(fun (x, _, _) -> x)) in
      let level_bad = concat_lsb (List.map per_pin ~f:(fun (_, x, _) -> x)) -- "level_bad" in
      let count_bad = concat_lsb (List.map per_pin ~f:(fun (_, _, x) -> x)) -- "count_bad" in
      let check_fix = resync_pins |: level_bad |: count_bad in
      (* Capture balance: every capture read is one merged toggle, i.e. one
         edge. An event attributing k pins is followed by k - 1 empty
         captures (the other pins' own toggles). Edges that cancelled in the
         XOR tree have no capture, so the next capture's snapshot attributes
         them with its own (wrong) time: once everything is drained and
         stable, captures - attributions is then negative. The pins
         attributed since the balance was last 0 are flagged.
         A read adds 1 - k (k <= pins), and reads between checks are
         unbounded (the FIFO need not empty), so no width holds every value.
         Without errors the balance stays in [1 - pins, 0]; it is [bw] bits
         signed, [2^(bw-1) > pins]: it saturates at the top, and once it
         reaches the floor [-2^(bw-1)] (< 1 - pins: unmatched attributions)
         it stays there until the check, which flags. *)
      let bw = num_bits_to_represent pins + 1 in
      let balance = wire bw in
      let unconfirmed = wire pins in
      let all_stable =
        lv ==: lv_p &: (ec ==: ec_p) &: (guard ==:. 0)
      in
      let balance_check = do_check &: all_stable in
      let attributed = popcount (changed |: reconfig) in
      let balance_read =
        let floor = of_int_trunc ~width:(bw + 1) (-(1 lsl (bw - 1))) in
        let top = of_int_trunc ~width:(bw + 1) ((1 lsl (bw - 1)) - 1) in
        let sum = sresize balance ~width:(bw + 1) +:. 1 -: uresize attributed ~width:(bw + 1) in
        let sat = mux2 (sum <+ floor) floor (mux2 (sum >+ top) top sum) in
        mux2 (balance ==: sel_bottom floor ~width:bw) balance (sel_bottom sat ~width:bw)
      in
      (* Excess captures (balance > 0: pulses narrower than the merge
         window, two empty captures) are caught by the pin's edge count. *)
      let merge_bad = balance_check &: msb balance in
      let merge_error =
        repeat merge_bad ~count:pins &: mux2 (unconfirmed ==:. 0) en unconfirmed
        -- "merge_error"
      in
      let untimed = level_bad <>:. 0 in
      (* Guard release: timer expired and the FIFO drained. *)
      let release = do_check &: (guard_timer ==:. 0) &: (guard <>:. 0) in
      (* Resync: pointers, merged level and levels stable for two samples. *)
      let stable_all =
        wr ==: wr_p &: (wf ==: wf_p) &: (mg ==: mg_p) &: (lv ==: lv_p) &: valid_r &: valid_f
      in
      let resync_done = resync &: stable_all in
      let late_ovf_w = wire 1 in
      let start_resync = fault_now |: ovf_now |: late_ovf_w in
      (* --- Pipeline stage A (registered at the read) --- *)
      let stage_a =
        { Stage.valid = do_read |: untimed
        ; untimed = ~:do_read &: untimed
        ; bank = next_f
        ; index = rd
        ; pins = mux2 do_read record.pins lv
        ; changed = mux2 do_read changed level_bad
        ; reconfig = mux2 do_read reconfig (zero pins)
        ; stale = mux2 do_read stale (zero pins)
        ; overflow = gnd
        ; fault = gnd
        }
      in
      (* Only the ring part of the record goes on, to the decoder. *)
      let ring_a =
        R.State.Of_signal.reg
          spec
          { R.State.nodes = record.nodes; count_a = record.count_a; count_b = record.count_b }
      in
      (* Marks from overflow/fault detection reach every event in flight. *)
      let mark_ovf = wire 1 in
      let mark_fault = wire 1 in
      let stage_reg (s : Signal.t Stage.t) = Stage.Of_signal.reg specc s in
      let marked (s : Signal.t Stage.t) =
        { s with overflow = s.overflow |: mark_ovf; fault = s.fault |: mark_fault }
      in
      let sa = stage_reg (marked stage_a) in
      let length_sel_q = r (r length_sel) in
      let sb = stage_reg (marked sa) in
      let dec_b = R.Decoded.Of_signal.reg spec (R.decode ~length_sel:length_sel_q ring_a) in
      let sc = stage_reg (marked sb) in
      let dec_d = R.Decoded.Of_signal.reg spec dec_b in
      (* Overwrite check: the pointer sample registered two edges after the
         read covers every write that landed before the read edge (a write
         just before it may resolve late in the first synchroniser stage).
         Has the writer reached [index + slots], i.e. reused the slot? *)
      let w_idx = mux2 sc.bank idx_f idx_r in
      let late_ovf =
        sc.valid &: ~:(sc.untimed) &: (Pointer.sub_mod ~slots w_idx sc.index >:. slots)
      in
      let sd = marked { sc with overflow = sc.overflow |: late_ovf } in
      Signal.assign late_ovf_w late_ovf;
      Signal.assign mark_ovf (ovf_now |: late_ovf);
      Signal.assign mark_fault fault_now;
      (* Order check. *)
      let tw = R.time_width in
      let modulus =
        mux
          length_sel_q
          (List.map R.lengths ~f:(fun n ->
             of_int_trunc ~width:(tw + 1) ((1 lsl R.count_width) * 2 * n))
           @ List.init
               ((1 lsl R.select_width) - List.length R.lengths)
               ~f:(fun _ ->
                 of_int_trunc ~width:(tw + 1) ((1 lsl R.count_width) * 2 * List.last_exn R.lengths)))
      in
      let have_last = wire 1 in
      let last_time = wire tw in
      let t = uresize dec_d.time ~width:(tw + 1) in
      let tl = uresize last_time ~width:(tw + 1) in
      let d = mux2 (t <: tl) (t -: tl +: modulus) (t -: tl) in
      let backward = sll d ~by:1 >: modulus in
      let back_by = modulus -: d in
      let timed = sd.valid &: ~:(sd.untimed) in
      (* Only events read within 4 cycles of each other are compared: their
         captures are then well within half the ring time range, which must
         exceed ~8 clock cycles. *)
      let since_last = wire 3 in
      let order_error =
        timed &: have_last &: (since_last <:. 4) &: backward &: (back_by >:. order_tolerance)
      in
      Signal.assign
        since_last
        (rc (mux2 timed (zero 3) (mux2 (since_last ==:. 7) since_last (since_last +:. 1))));
      let nonempty = sd.changed |: sd.reconfig |: sd.stale <>:. 0 in
      let out_valid = sd.valid &: (sd.untimed |: nonempty) in
      let empty_event = timed &: ~:nonempty in
      Signal.assign have_last (rc (mux2 timed vdd (have_last &: ~:resync_done)));
      Signal.assign last_time (rc (mux2 timed dec_d.time last_time));
      (* --- State updates --- *)
      Signal.assign resync (rc ~clear_to:vdd (mux2 resync (~:resync_done) start_resync));
      let rd_next which idx_now rd_cur =
        mux2 resync_done idx_now
          (mux2 (do_read &: (if which then next_f else ~:next_f)) (Pointer.inc_mod ~slots rd_cur) rd_cur)
      in
      Signal.assign rd_r (rc (rd_next false idx_r rd_r));
      Signal.assign rd_f (rc (rd_next true idx_f rd_f));
      Signal.assign next_f (rc (mux2 resync_done mg (next_f ^: do_read ^: runt)));
      Signal.assign
        prev_snap
        (rc
           (mux2 resync_done lv
              (mux2 do_read record.pins
                 (* Level and resync fixes: take the sampled level. *)
                 (check_fix &: lv |: (~:check_fix &: prev_snap)))));
      Array.iteri cnt ~f:(fun i c ->
        (* A stale report is one edge without a time. *)
        let attributed = c +: uresize (do_read &: (changed.:(i) |: stale.:(i))) ~width:2 in
        Signal.assign c (rc (mux2 check_fix.:(i) (ec_count i) attributed)));
      Signal.assign
        need_resync
        (rc ~clear_to:(ones pins)
           (mux2 resync_done (ones pins)
              (need_resync &: ~:resync_pins |: mux2 release guard (zero pins))));
      let cfg = r pin_enable in
      let en_change = cfg ^: en in
      Signal.assign en (rc cfg);
      Signal.assign guard (rc (mux2 release (zero pins) guard |: en_change));
      Signal.assign
        guard_timer
        (rc
           (mux2 (en_change <>:. 0)
              (of_int_trunc ~width:(width guard_timer) guard_cycles)
              (mux2 (guard_timer ==:. 0) guard_timer (guard_timer -:. 1))));
      Signal.assign
        balance
        (rc
           (mux2 (resync_done |: balance_check) (zero bw)
              (mux2 do_read balance_read balance)));
      Signal.assign
        unconfirmed
        (rc
           (mux2 (resync_done |: balance_check |: (do_read &: (balance_read ==:. 0))) (zero pins)
              (mux2 do_read (unconfirmed |: changed) unconfirmed)));
      Signal.assign
        alt_wait
        (rc (mux2 (normal &: alt_transient &: ~:runt) (alt_wait +:. 1) (zero 2)));
      (* Auto-reset of a pointer that stays invalid while resynchronising. *)
      let bad_streak = r (resync &: pointer_bad) in
      Signal.assign
        ar_cnt
        (rc
           (mux2 (ar_cnt <>:. 0) (ar_cnt -:. 1)
              (mux2 (resync &: pointer_bad &: bad_streak) (of_int_trunc ~width:3 7) ar_cnt)));
      let ptr_reset = r (clear |: (ar_cnt >:. 4)) -- "ptr_reset" in
      let edge_reset = r clear in
      (* --- Status --- *)
      let sticky x = reg_fb specc ~width:(width x) ~f:(fun s -> mux2 clear_status (zero (width x)) (s |: x)) in
      let counter x =
        reg_fb specc ~width:status_width ~f:(fun c ->
          mux2 clear_status (zero status_width) (c +: uresize x ~width:status_width))
      in
      let count_bits x = popcount x in
      let status =
        { Status.resyncing = resync
        ; capture_fault = sticky fault_now
        ; fault_count = counter fault_now
        ; overflow = sticky mark_ovf
        ; overflow_count = counter (ovf_now |: late_ovf)
        ; lost_edge = sticky count_bad
        ; lost_edge_count = counter (count_bits count_bad)
        ; level_mismatch = sticky level_bad
        ; level_mismatch_count = counter (count_bits level_bad)
        ; merge_error = sticky merge_error
        ; merge_error_count = counter merge_bad
        ; empty_count = counter empty_event
        ; stale_count = counter (popcount sd.stale &: repeat timed ~count:(num_bits_to_represent pins))
        ; fault_now = r fault_now
        ; runt_now = r runt
        ; runt_count = counter runt
        ; overflow_now = r mark_ovf
        ; lost_edge_now = r count_bad
        ; level_mismatch_now = r level_bad
        ; merge_error_now = r merge_error
        ; ptr_reset
        ; enabled = en
        ; guarded = guard
        }
      in
      let event =
        { Event.valid = out_valid
        ; untimed = sd.untimed
        ; pins = sd.pins
        ; changed = sd.changed
        ; reconfig = sd.reconfig
        ; stale = sd.stale
        ; bank = sd.bank
        ; time = dec_d.time
        ; phase = dec_d.phase
        ; periods = dec_d.periods
        ; pattern_error = dec_d.pattern_error &: timed
        ; order_error
        ; overflow = sd.overflow
        ; fault = sd.fault
        }
      in
      let raw =
        Record.Of_signal.unpack
          (mux raw_select (List.map (front.slots_r @ front.slots_f) ~f:Record.Of_signal.pack))
      in
      ( { O.event; status; raw }
      , { enable = en; ptr_reset_n = ~:ptr_reset; edge_reset_n = ~:edge_reset; ptr_reset } )
    ;;
  end

  (** [create ?first_stage ?name implementation scope i]. [name] (default
      ["pin_capture"]) prefixes every instance. [first_stage]: build the
      synchronisers' first stage from flop cells instead of kept Hardcaml
      registers. *)
  let create ?first_stage ?(name = "pin_capture") implementation scope (i : Signal.t I.t) =
    I.Of_signal.assert_widths i;
    let enable = wire pins in
    let ptr_reset_n = wire 1 in
    let edge_reset_n = wire 1 in
    let ptr_reset = wire 1 in
    let front =
      match implementation with
      | Cells c ->
        Front_cells.create ~name c ~pins:i.pins ~enable ~ptr_reset_n ~edge_reset_n ~ring:i.ring
      | Model s -> Front_model.create ~clock:i.clock ~ptr_reset s
    in
    let o, (ctl : Drain.controls) =
      Drain.create
        ?first_stage
        ~name
        scope
        ~clock:i.clock
        ~clear:i.clear
        ~clear_status:i.clear_status
        ~pin_enable:i.pin_enable
        ~length_sel:i.length_sel
        ~raw_select:i.raw_select
        front
    in
    Signal.assign enable ctl.enable;
    Signal.assign ptr_reset_n ctl.ptr_reset_n;
    Signal.assign edge_reset_n ctl.edge_reset_n;
    Signal.assign ptr_reset ctl.ptr_reset;
    o
  ;;

  (** As [create], as a separate module called [module_name] (the caller's
      prefix included, e.g. ["protoemu_pin_capture"]) unless the scope
      flattens the design. [Model] needs a flattened scope. *)
  let hierarchical ?first_stage ?name ~module_name implementation scope =
    (match implementation with
     | Model _ when not (Scope.flatten_design scope) ->
       raise_s
         [%message
           "Pin_capture.Model is simulation-only: use it in a scope with ~flatten_design:true"
             (module_name : string)]
     | Model _ | Cells _ -> ());
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:module_name (create ?first_stage ?name implementation)
  ;;
end
