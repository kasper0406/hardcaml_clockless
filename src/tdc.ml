(** A delay-line time-to-digital converter (TDC): timestamps edges on a 1-bit
    input with sub-clock resolution.

    {v
                   analog part: Cells (hand-placed) or Model (Cyclesim)
     hit --+--> [stage0] --+--> [stage1] --+-- ... --> [stage L-1] --+
           |               |               |                         |
       [sample0]       [sample1]       [sample2]        ...      [sampleL]   clock
     ------+---------------+-------- sampled, L+1 bits --------------+------------
                                           |
                       decoder: ordinary synchronous Hardcaml
       sync regs -> compare with level -> majority filter -> first run -> out regs
    v}

    The first sample stage is a bank of plain registers named
    [<name>_sample<i>] with [dont_touch]/[keep], or, with [first_stage], of
    flip-flop cells: instances [<name>_sample<i>], outputs
    [<name>_sample<i>_q]. Synthesis gives a kept register a generated
    instance name (only its output net keeps the name), so prefer cells
    where constraints must find the flops. The delay cells are
    [<name>_stage<i>] (see {!Delay_line}).

    {2 Positions}

    The sampled vector has [length + 1] {e positions}: position 0 is [hit]
    itself and position [i >= 1] is the delay line's tap after [i] cells
    ([Delay_line] [taps.(i - 1)]). At a clock edge, position [i] shows the
    input as it was [i] stage delays earlier. An edge that reached the input
    [k] stage delays before the clock edge (more precisely, between [k] and
    [k + 1] delays) has flipped positions [0..k] and no others: [fine = k].
    Arrival time = time of the sampling clock edge - [fine] stage delays, so a
    larger [fine] means an earlier edge. Stage delays differ from cell to cell
    and with PVT, so the bins are not uniform; code-density calibration (not
    here) turns codes into time.

    {2 Decoder}

    - The settled level is position 0 of the previous sample. An edge occurred
      in the last cycle iff position 0 differs from it ([valid]); [rising] is
      the new level. Position 0 is a plain synchroniser: an edge that arrives
      right at the clock edge may be reported in either cycle, with [fine] 0
      in this one or [fine] = (stages per cycle) - 1 in the next, which is the
      same time to one bin (if the line covers a full cycle).
    - [d = sampled XOR level]: 1 where a position shows the new level.
    - Bubble filter: each position [1..length] is replaced by the majority of
      itself and its two neighbours (the position past the end counts as the
      old level). Position 0 is not filtered (it alone decides [valid]).
      A single bubble at any position [>= 1] moves [fine] by at most 1, and
      not at all if it is 2 or more positions from the transition. Pulses
      narrower than 2 stages are filtered away.
    - [fine] = length of the initial run of the filtered vector minus 1, i.e.
      where the most recent edge got to. Only that first run is used, so the
      older history that a line longer than a cycle still holds beyond the
      edge does not disturb the code (a popcount of [d] would).
    - [overflow]: every position shows the new level, so the edge entered the
      line at least [length] stage delays before the clock edge. [fine] is
      then [length] and only bounds the arrival time.
    - [multi_edge]: more than one edge happened inside the first
      [multi_edge_window] positions, i.e. a glitch narrower than a cycle. This
      is flagged rather than decoded: [valid] still says whether the level
      changed overall, and [fine] is the position of the most recent edge.
      Limits: only edges within the window are seen; a pulse that both
      starts and ends further back than the window (or the line) is lost
      silently; pulses narrower than 2 stages are filtered away.
    - Edges (anywhere in the line, including earlier cycles' edges still in
      it) must be at least 2 stages apart to be resolved; with a bubble next
      to them, 3 (the tested bound).
    - [coarse]: a free-running cycle counter. [coarse = n] for the [n]-th
      clock edge after [clear] is released (the first is 0), counting the
      sampling edge, modulo [2 ^ coarse_width].

    {2 Choosing [length]}

    The line should span at least one clock period at the fastest corner;
    otherwise edges early in a cycle are reported with [overflow] (time known
    only to the cycle). It may span more: history beyond the current cycle is
    ignored. [multi_edge_window] must then be at most the number of stages per
    period at the slowest corner.

    {2 Output timing}

    All outputs are registered; an event sampled at edge [n] appears on the
    outputs after edge [n + 2] (one synchroniser stage, one output stage).
    Edges sampled while [clear] is high are not reported. [fine], [rising],
    [overflow] and [coarse] are 0 when neither [valid] nor [multi_edge] is
    set. *)

open! Base
open Hardcaml
open Signal

module type Config = sig
  (** Number of delay cells. Positions are [0..length]. *)
  val length : int

  (** Width of the coarse cycle counter. *)
  val coarse_width : int

  (** Positions [0 .. multi_edge_window - 1] are searched for a second edge.
      It must not exceed the number of stage delays per clock cycle at the
      slowest corner: further back, the line legitimately holds edges from
      earlier cycles, which would be flagged falsely. 1 disables the check;
      [length + 1] is right when the line is shorter than a cycle at every
      corner. *)
  val multi_edge_window : int
end

module type S = sig
  include Config

  (** Number of sampled positions, [length + 1]. *)
  val positions : int

  (** Bits needed for [0..length]. *)
  val fine_width : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; hit : 'a (** the input whose edges are timestamped; unused in [Model] mode *)
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { valid : 'a (** the input changed level since the previous sample *)
      ; rising : 'a (** the input's level after the most recent edge *)
      ; fine : 'a (** stage delays between the most recent edge and the sampling edge *)
      ; coarse : 'a (** index of the sampling clock edge *)
      ; overflow : 'a (** the edge passed the whole line; [fine = length] is a lower bound *)
      ; multi_edge : 'a (** more than one edge in the window: a glitch narrower than a cycle *)
      }
    [@@deriving hardcaml]
  end

  (** Simulation-only stimulus for [Model]: what happened at the line input
      during the cycle that ends at this clock edge. *)
  module Model_stimulus : sig
    type 'a t =
      { edge : 'a (** an edge arrived during this cycle *)
      ; rising : 'a (** the level the input changed to *)
      ; stages : 'a
      (** it arrived this many stage delays before the clock edge (flips
          positions [0..stages]). Must be below [stages_per_cycle]; values
          [>= length] all mean "passed the whole line". *)
      ; glitch : 'a (** the input went back to its previous level later in the cycle *)
      ; glitch_stages : 'a (** ... this many stage delays before the edge; [< stages] *)
      ; bubbles : 'a
      (** XOR-ed into the sampled positions (sampling errors, which do not
          propagate along the line) *)
      }
    [@@deriving hardcaml]
  end

  type implementation =
    | Cells of Cell_library.t * string
    (** Hand-placed delay cells of the given (buffer or delay) cell type, for
        Verilog generation. Cyclesim cannot simulate it. *)
    | Model of
        { stages_per_cycle : int
        ; stimulus : Signal.t Model_stimulus.t
        }
    (** A behavioural model for Cyclesim (see {!Model}). *)

  (** The analog part: delay line plus the first sample stage. *)
  module Analog : sig
    (** [create ?first_stage ~name implementation ~clock ~hit] returns the
        [positions]-bit sampled vector, bit [i] = position [i], registered on
        [clock]. The sampling registers are named [<name>_sample<i>] and
        carry [dont_touch]/[keep]; with [first_stage] they are flip-flop
        cells instead (instances [<name>_sample<i>], outputs
        [<name>_sample<i>_q], reset tied inactive). Delay cells are
        [<name>_stage<i>]. *)
    val create
      :  ?first_stage:Cell_library.t * Cell.Flop.t
      -> name:string
      -> implementation
      -> clock:Signal.t
      -> hit:Signal.t
      -> Signal.t
  end

  (** The synchronous decoder. Ordinary Hardcaml: it simulates in Cyclesim
      and can be checked formally on its own. *)
  module Decoder : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; sampled : 'a (** from {!Analog.create} *)
        }
      [@@deriving hardcaml]
    end

    val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  end

  (** [create ?first_stage ?name implementation scope i]. [name] (default
      ["tdc"]) prefixes cell instance and sampling register names.
      [first_stage]: build the first sample stage from flip-flop cells (see
      {!Analog.create}). *)
  val create
    :  ?first_stage:Cell_library.t * Cell.Flop.t
    -> ?name:string
    -> implementation
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  (** As [create], as a separate module called [module_name] (the caller's
      prefix included, e.g. ["protoemu_tdc"]) unless the scope flattens the
      design. [Model] is only allowed in a flattened scope, since its stimulus
      comes from outside the module. *)
  val hierarchical
    :  ?first_stage:Cell_library.t * Cell.Flop.t
    -> ?name:string
    -> module_name:string
    -> implementation
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end

(** Behavioural model of the analog part, for Cyclesim.

    It is NOT a timing model of the circuit. It assumes every stage has the
    same delay, an integer number [stages_per_cycle] of stage delays per clock
    cycle, at most two edges per cycle, ideal sampling (no metastability) and
    errors only where [bubbles] injects them. It keeps the line's contents
    between cycles, so when the line is longer than a cycle
    ([stages_per_cycle <= length]) positions [>= stages_per_cycle] show the
    previous cycles' input, as a real line would. Its purpose is to let the
    decoder, and the rest of a design, simulate in Cyclesim with codes that
    have the shape a real line produces. Silicon behaviour (non-uniform bins,
    PVT, bubbles' real statistics) must come from calibration and gate-level
    simulation. *)
module Model = struct
  let line ~positions ~stages_per_cycle ~clock ~edge ~rising ~stages ~glitch ~glitch_stages =
    let width = Signal.width stages in
    let now = wire positions in
    let prev = reg (Reg_spec.create ~clock ()) now in
    (* Level at the start of the cycle = input at the previous sample. *)
    let start = prev.:(0) in
    let position i =
      let i' = of_int_trunc ~width i in
      let history = if i < stages_per_cycle then start else prev.:(i - stages_per_cycle) in
      mux2
        (edge &: glitch &: (i' <=: glitch_stages))
        start
        (mux2 (edge &: (i' <=: stages)) rising history)
    in
    Signal.assign now (concat_lsb (List.init positions ~f:position));
    now
  ;;
end

let raise_config msg = raise_s [%message "Tdc: bad configuration" (msg : string)]

module Make (Config : Config) : S = struct
  include Config

  let () =
    if length < 1 then raise_s [%message "Tdc: length must be at least 1" (length : int)];
    if coarse_width < 1
    then raise_s [%message "Tdc: coarse_width must be at least 1" (coarse_width : int)];
    if multi_edge_window < 1 || multi_edge_window > length + 1
    then
      raise_s
        [%message
          "Tdc: multi_edge_window must be in [1, length + 1]"
            (multi_edge_window : int)
            (length : int)]
  ;;

  let positions = length + 1
  let fine_width = num_bits_to_represent length

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; hit : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid : 'a
      ; rising : 'a
      ; fine : 'a [@bits fine_width]
      ; coarse : 'a [@bits coarse_width]
      ; overflow : 'a
      ; multi_edge : 'a
      }
    [@@deriving hardcaml]
  end

  module Model_stimulus = struct
    type 'a t =
      { edge : 'a
      ; rising : 'a
      ; stages : 'a [@bits fine_width]
      ; glitch : 'a
      ; glitch_stages : 'a [@bits fine_width]
      ; bubbles : 'a [@bits positions]
      }
    [@@deriving hardcaml]
  end

  type implementation =
    | Cells of Cell_library.t * string
    | Model of
        { stages_per_cycle : int
        ; stimulus : Signal.t Model_stimulus.t
        }

  module Analog = struct
    let sample_attributes = Cell.attributes

    let create ?first_stage ~name implementation ~clock ~hit =
      if Signal.width hit <> 1 then raise_config "hit must be 1 bit";
      (* What each position shows at the clock edge. *)
      let at_edge =
        match implementation with
        | Cells (library, cell) ->
          let line = Delay_line.create library ~name ~cell ~length hit in
          hit :: Array.to_list line.taps
        | Model { stages_per_cycle; stimulus = s } ->
          if stages_per_cycle < 1 then raise_config "stages_per_cycle must be at least 1";
          Model_stimulus.Of_signal.assert_widths s;
          let clean =
            Model.line
              ~positions
              ~stages_per_cycle
              ~clock
              ~edge:s.edge
              ~rising:s.rising
              ~stages:s.stages
              ~glitch:s.glitch
              ~glitch_stages:s.glitch_stages
          in
          bits_lsb (clean ^: s.bubbles)
      in
      let spec = Reg_spec.create ~clock () in
      List.mapi at_edge ~f:(fun i x ->
        let nm = Printf.sprintf "%s_sample%d" name i in
        match first_stage with
        | None ->
          let r = reg spec x in
          List.iter sample_attributes ~f:(fun a -> ignore (add_attribute r a : Signal.t));
          r -- nm
        | Some (library, f) ->
          (* the instance keeps [nm]; a wire of the same name would rename it *)
          Cell.Flop.instantiate library f ~instance:nm ~clk:clock ~reset_n:vdd x -- (nm ^ "_q"))
      |> concat_lsb
    ;;
  end

  module Decoder = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; sampled : 'a [@bits positions]
        }
      [@@deriving hardcaml]
    end

    let create scope (i : Signal.t I.t) : Signal.t O.t =
      I.Of_signal.assert_widths i;
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create ~clock:i.clock () in
      let spec_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      (* Second register stage: resolves metastability of the first. *)
      let s = reg spec i.sampled -- "sync" in
      (* The settled level: position 0 of the previous sample. *)
      let level = reg spec s.:(0) -- "level" in
      let d = s ^: repeat level ~count:positions in
      let dbit j = if j >= positions then gnd else d.:(j) in
      let maj a b c = a &: b |: (a &: c) |: (b &: c) in
      let m =
        concat_lsb
          (List.init positions ~f:(fun j ->
             if j = 0 then dbit 0 else maj (dbit (j - 1)) (dbit j) (dbit (j + 1))))
        -- "filtered"
      in
      let edge = m.:(0) in
      let run = trailing_ones ~:(m ^: repeat edge ~count:positions) in
      let fine = uresize (run -:. 1) ~width:fine_width in
      let overflow = edge &: (m ==: ones positions) in
      let multi_edge =
        List.init (multi_edge_window - 1) ~f:(fun j -> ~:(m.:(j)) &: m.:(j + 1))
        |> List.fold ~init:gnd ~f:( |: )
      in
      (* Samples taken while [clear] was high are not reported. *)
      let sampled_in_clear = pipeline spec ~n:2 i.clear in
      let report = ~:sampled_in_clear in
      let valid = edge &: report in
      let multi_edge = multi_edge &: report in
      let any = valid |: multi_edge in
      let count =
        (* -2 at clear, so the count is 0 when the first sample after clear
           reaches the output register. *)
        reg_fb
          spec_clear
          ~clear_to:(of_int_trunc ~width:coarse_width (-2))
          ~width:coarse_width
          ~f:(fun c -> c +:. 1)
        -- "count"
      in
      let r x = reg spec x in
      { O.valid = r valid
      ; rising = r (s.:(0) &: any)
      ; fine = r (fine &: repeat any ~count:fine_width)
      ; coarse = r (count &: repeat any ~count:coarse_width)
      ; overflow = r (overflow &: valid)
      ; multi_edge = r multi_edge
      }
    ;;
  end

  let create ?first_stage ?(name = "tdc") implementation scope (i : Signal.t I.t) =
    let sampled = Analog.create ?first_stage ~name implementation ~clock:i.clock ~hit:i.hit in
    Decoder.create scope { Decoder.I.clock = i.clock; clear = i.clear; sampled }
  ;;

  let hierarchical ?first_stage ?name ~module_name implementation scope =
    (match implementation with
     | Model _ when not (Scope.flatten_design scope) ->
       raise_s
         [%message
           "Tdc.Model is simulation-only: use it in a scope with ~flatten_design:true"
             (module_name : string)]
     | Model _ | Cells _ -> ());
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:module_name (create ?first_stage ?name implementation)
  ;;
end
