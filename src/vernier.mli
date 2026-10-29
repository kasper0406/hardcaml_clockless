(** Output edge vernier: places each edge of an output pin at a fraction of
    the system clock cycle. This is the chip's D-3 vernier; the contract it
    implements is janestreet-bitbanger's TIMING_ISA.md ("Vernier") and
    ISA_V1_SEMANTICS.md D33.

    {v
     per pin:
       level, fraction --> control (clk) --+--> rise line --+--> line_out
                           map, select,    |                XOR --> loopback
                           settle timers   +--> fall line --+  |
                                           |                   +--> A0 \
                                           +--> level flop ----------> A1  out mux --> pin
                                           +--> bypass | fallback -----> S /

     per line:
       launch flop -> coarse chain: [coarse_stage] x (coarse_taps - 1), tap 0 = chain input
                   -> mux2 tree on taps (coarse select) -> inv --> A0 \
                   -> inv -----------------------------------> A1  coarse-bypass mux
                   -> fine chain: [fine_stage] x (fine_taps - 1)
                   -> mux2 tree on taps (fine select) -------> A0 \
                   -> fine chain input ------------------------> A1  fine-bypass mux -> XOR
    v}

    {2 Lines}

    Each pin has a {e rise line}, which times every rising edge of the pin,
    and a {e fall line}, which times every falling edge. An edge toggles its
    line's launch flop; the XOR of the two line outputs is the pin's level.
    One output edge is one toggle of one XOR input, so the XOR never
    glitches, and the two edges of a pulse are placed independently, each by
    its own line, and may be in flight together.

    The coarse chain is meant to span at least one clock period at the
    fastest corner, the fine chain at least one coarse step at the slowest.
    With [dlygate4sd3, inv, dlygate4sd3, inv] stages (taps after every
    stage, so all taps have the input's polarity) every coarse step contains
    one rising and one falling transition of each cell, whichever way the
    edge goes, which cancels the cells' rise/fall difference (3.5% for a plain
    dlygate4sd3: 0.7 ns over a 20 ns span). The inverter after the coarse
    tree puts about as many muxes after it as before it in full mode, so the
    muxes' rise/fall difference (22 ps per level) roughly cancels too: in
    Event_sim the two launch directions of a line differ by 5-42 ps in full
    mode (the fall lines, on the XOR's second input, have the larger values;
    60-175 ps in the other modes, which are not balanced). The calibration
    averages the two directions; there is no per-direction constant, so an
    unchanged fraction keeps an unchanged select in both directions.

    The mux trees have equal depth: an unpaired last node feeds both inputs
    of its mux, so every tap goes through the same number of muxes.

    {2 Modes}

    Per pin, 2 bits ({!Mode.to_int}); bypass after [clear]:
    - [Bypass] (0): the pin is the level flop through the output mux:
      synchronous, cycle resolution, fraction ignored. The mux is the last
      cell before the pin, so nothing in the lines can affect a bypassed pin.
      The lines keep running in full configuration (the XOR parity stays
      equal to the level, and a line can be measured through [line_out] while
      its pin is bypassed).
    - [Coarse] (1): the fine section is bypassed; the fraction is mapped
      through the calibration to the nearest coarse tap.
    - [Fine] (2): diagnostic. The coarse section is bypassed and the
      fraction's low bits select a fine tap directly, uncalibrated; the
      range is the fine chain's span (about 1.5 coarse steps).
    - [Full] (3): both sections, through the calibration.

    The bypass muxes go around a whole section, so a broken coarse chain or
    tree leaves fine mode (and bypass) working, a broken fine chain leaves
    coarse mode, and a broken XOR still leaves bypass.

    {2 Timing}

    A request is registered at clock edge [n] ([level] differs from the
    previous request: an edge). The target line's tap select is updated at
    edge [n + 1] and its launch flop toggles at edge [n + 2]. In coarse and
    full mode the pin then moves at [edge (n + 2) + d_align + fraction * T /
    2^fraction_bits], where [d_align] is the common zero-fraction delay the
    calibration aligns every line to ({!Calibration.t.d_align_ps}: 1.5-3.2
    ns before layout); in bypass at [edge (n + 2)] plus the level flop's
    clk->Q and the output mux. The integrator issues requests two cycles
    early or delays the other pins to match.

    {2 Safe select updates and the edge-rate limit}

    A line holds an edge until it has left both chains, whatever tap is
    selected: up to 56.8 ns at the slow corner before layout (Liberty "1k"
    wire load; 2.8 cycles at 50 MHz). Changing a select while an edge is
    anywhere in the line can move the output back and forth, so selects
    change only at an edge where the line is {e quiet}: no launch at that
    edge and at least [settle_cycles] edges since its last launch. The launch
    follows one cycle later, so the new select has a whole cycle to reach the
    muxes. [settle_cycles * T] must cover the worst line delay plus clk->Q,
    clock skew and select fan-out: at 50 MHz, 3 before layout (5% margin),
    4 with margin. Both bounds are signoff STA checks ({!Sdc.vernier}, given
    the clock period and the smallest [settle_cycles] the host will load):
    every line mux select within one period of the clock edge, and every
    path from a launch flop through its line and the XOR to the output mux
    within [settle_cycles] periods. The host must never load a smaller
    [settle_cycles], nor run a faster clock, than the SDC was checked for.

    Hence, per line (that is, between edges of the same direction on a pin):
    an edge that needs a different select must come at least
    [B = settle_cycles + 1] cycles after the line's previous edge (4, or 5
    with margin, at 50 MHz); an edge whose select is unchanged (same
    fraction in the same mode, e.g. a clock at a fixed phase) may come at any
    time, so a pin can toggle every cycle. The rise and fall lines are
    independent, so a pulse can be shorter than a cycle.

    {2 Faults}

    - {e Overrun}: an edge that breaks the rule above, in a vernier mode. Its
      line still toggles (with its old select, so the XOR parity stays right),
      but the pin drops to bypass at once: it shows the requested level at
      cycle resolution (an edge in flight may move to cycle resolution) and
      never keeps a wrong level.
    - {e Range fault}: in coarse or full mode, a fraction whose mapped
      position lies past the last tap (at clocks where the line is shorter
      than a cycle). The tap is clamped and the pin drops to bypass.
    Both set sticky per-pin flags ([overrun], [range_fault]; one-cycle
    [overrun_event]/[range_event] pulses for counters) and put the pin in
    bypass ([fallback]). Acknowledging and re-arming are separate:
    [clear_flags] clears the flags only, and the pin stays in bypass until
    the host writes its mode again ([mode_write], also with an unchanged
    value; in the chip only while every engine is held). The re-arm applies
    together with the mode, at the first edge where a mode change may apply
    (below), whether or not the flags were cleared.

    A host change of [mode] (and a re-arm) applies at the first edge where
    both lines of the pin have {e settled} ([settle_cycles + 2] edges since
    their last launch: the drain bound already covers the XOR, which the
    output mux switches to, and the two cycles are margin beyond it) and no
    request is being mapped;
    in the chip, mode changes happen while every engine is held, so at once.
    The mode applied is the [mode] input at that edge, so [mode] must hold
    the written value by the time [mode_write] is high. A fault discards a
    pending re-arm, including a mode write in the fault's cycle: re-arming
    after it takes a new write. With healthy lines both inputs of every
    switched mux are then equal and the pin does not move.
    This, like every select change, assumes the mux2 cell does not glitch
    when its select changes with equal data inputs (true of a
    transmission-gate mux; check the cell).

    [clear] resets the launch and level flops asynchronously and the control
    synchronously, and selects bypass; the lines drain within [settle_cycles]
    cycles after.

    {2 Pulses}

    Both edges of a pulse pass the XOR and the output mux: in Event_sim
    pulses below about 0.3 ns (typ, fast) to 0.6 ns (slow) are swallowed
    rather than reaching the pin as runts, and the delivered width carries
    the difference of the two lines' calibration errors (up to about 0.3 ns
    with 3% cell variation): requests of 1 ns and more are reproduced, below
    that they may come out narrower or not at all.

    {2 Mapping and calibration constants}

    Per line [l] (pin [p]: [2p] rise line, [2p + 1] fall line), host-loaded:
    - [scale]: coarse taps per cycle, unsigned, {!scale_frac_bits} fraction
      bits. 0 is the {e identity}: in coarse and full mode the fraction is
      the coarse tap (fine 0), which is how calibration sweeps the taps.
    - [offset]: added position, in 1/2^{!position_frac_bits} coarse steps
      ({!offset_bits} bits, up to 4 steps): aligns the line to [d_align].
    - [ratio]: fine taps per coarse step, R = 8 + [ratio] / 2 (8 .. 15.5).
    The hardware trusts these: a [scale] that is not the line's coarse taps
    per cycle scales every delay by the error, and a fraction it maps inside
    the line raises no range fault (the range check is against the taps,
    not the cycle). The [scale] field holds at most [2^coarse_bits - 1/16]
    taps per cycle (63.9 for 40 taps): at clocks slower than that many
    coarse steps (in Event_sim, 36 ns at the fast corner, 53 ns typical, 85
    ns slow; so below about 28 MHz at the fast corner) no correct constant
    exists and the pin must stay in bypass. {!Calibration.fit} refuses
    constants that do not fit their fields rather than clipping them.

    For a fraction [a] the position is
    [p = round(a * scale / 2^(fraction_bits + scale_frac_bits - position_frac_bits)) + offset],
    in 1/2^position_frac_bits coarse steps. Full: coarse = [p / 2^pf], fine
    = [round(rho * R)] with [rho] the remainder in coarse steps. Coarse:
    coarse = [p / 2^pf] (the calibration adds half a step to the offset, to
    round). Results past the last tap set the range fault.

    One mapper per pin (8 x (coarse bits + 4) multiplier, 6 x 4 for the fine
    code, adders; about 6k um^2 after yosys) is shared by the pin's two
    lines: a pin has at most one edge per cycle. There is no INL table: with
    3% random cell variation the linear fit leaves 34-71 ps rms, 100-219 ps
    worst error at 50 MHz (0.4-0.9 LSB rms, 1.3-2.8 LSB worst), mostly the
    coarse chain's random walk. The constants are inputs: the chip's
    host-config block holds them (4 x (scale + {!offset_bits} +
    {!ratio_bits}) = 88 bits for 2 pins).

    {b Calibration} (host software: {!Calibration.fit}): with identity
    constants, loop each line ([line_out]) into the capture block and sweep
    the fraction in full mode (the coarse taps) and in fine mode (the fine
    taps), sending rise, fall, rise, fall at each value, more than
    [settle_cycles + 2] cycles apart, so each line shows one edge in each
    launch direction. [fit] turns the edge times (from the launch clock edge)
    into constants for full or coarse mode. The loopback leaves out the pad
    and the I/O mux: it calibrates resolution, linearity and line alignment,
    not pin-referred absolute time.

    [mismatch] (sticky, cleared by [clear_flags]): with both lines quiet for
    [settle_cycles + 2] cycles, the synchronised XOR differs from the level:
    a broken line in the current configuration, in any mode (also while
    bypassed), or one slower than the drain bound. It misses an even number
    of lost toggles and a line stuck at the right level. The 2-flop
    loopback synchroniser gets the two cycles after the drain bound, which
    STA checks to the output mux, not to the synchroniser on the same XOR
    net: a line within a wire delay and a setup time of the bound may set a
    false [mismatch] (a diagnostic only; the pin is not affected).

    {2 Model}

    [Model] replaces the lines for Cyclesim: line delay
    [base + coarse * coarse_ticks + fine * fine_ticks] ticks (a bypassed
    section adds nothing), 2^(fraction_bits + 4) ticks per cycle. A launch at
    edge [m] with delay [d] toggles the line in the cycle after edge
    [m + d / ticks_per_cycle], at [d mod ticks_per_cycle] ticks into it
    ([model_toggle], [model_ticks]); [out], [loopback] and [line_out] are
    levels at the end of each cycle. Like {!Tdc.Model} it is not a timing
    model of the cells (uniform steps, no PVT); it lets the control and the
    rest of a design simulate. In [Cells] mode [model_toggle] and
    [model_ticks] are 0. *)

open! Base
open Hardcaml

(** A flip-flop cell with an active-low asynchronous reset. *)
type flop =
  { cell : string
  ; clk : string
  ; d : string
  ; reset_n : string
  ; q : string
  }

module Mode : sig
  type t =
    | Bypass
    | Coarse
    | Fine
    | Full
  [@@deriving sexp_of, equal, enumerate]

  (** The [mode] input encoding: 0, 1, 2, 3, as in bitbanger's VERNIER_CFG. *)
  val to_int : t -> int

  (** Sections an edge goes through (in bypass, the lines run in full
      configuration). *)
  val uses_coarse : t -> bool

  val uses_fine : t -> bool
end

val scale_frac_bits : int
val position_frac_bits : int
val offset_bits : int
val ratio_bits : int
val settle_bits : int
val model_extra_bits : int

module type Config = sig
  (** Vernier pins, 1..4. *)
  val pins : int

  (** Coarse taps, 2..64 (the chain has [coarse_taps - 1] stages). *)
  val coarse_taps : int

  (** Fine taps, 2..32. *)
  val fine_taps : int

  (** Bits of the cycle fraction, 3..12 (8 in bitbanger). *)
  val fraction_bits : int
end

type cells =
  { library : Cell_library.t
  ; coarse_stage : string list
  (** cells of one coarse stage, non-inverting overall (see {!Delay_line.create_stages}) *)
  ; fine_stage : string list
  ; inverter : string
  ; mux2 : string
  ; xor2 : string
  ; buffer : string
  (** for the controls into the cells: every select and the launch/level
      flop reset enters through an isolating buffer
      ([<line>_sel_{cm<l>,fm<l>,cbyp,fbyp}_iso], [<pin>_sel_out_iso],
      [<name>_reset_iso]; {!Cell.isolate}) and fans out within
      {!Cell.max_fanout} through a tree of these ([..._fo_<level>_<j>]):
      OpenROAD never repairs a protected net *)
  ; flop : flop (** launch and level flops *)
  }

(** Behavioural line delays, in ticks (see Model above). *)
type model =
  { base_ticks : int
  ; coarse_ticks : int
  ; fine_ticks : int
  }

type implementation =
  | Cells of cells
  (** Hand-placed cells for Verilog generation and Event_sim. Instances
      are [<name>_p<i>_{r,f}_launch], [..._c_stage<k>_<j>] (coarse chain),
      [..._cm<level>_<j>] (coarse tree), [..._cinv], [..._cbinv],
      [..._cbyp], [..._f_stage<k>_<j>], [..._fm<level>_<j>], [..._fbyp],
      and per pin [<name>_p<i>_xor], [<name>_p<i>_level], [<name>_p<i>_out]. *)
  | Model of model (** behavioural, for Cyclesim *)

module type S = sig
  include Config

  (** [2 * pins]: line [2p] is pin [p]'s rise line, [2p + 1] its fall line. *)
  val lines : int

  val coarse_bits : int
  val fine_bits : int
  val scale_bits : int
  val model_ticks_bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; level : 'a list (** per pin: the requested level; a change is an edge *)
      ; fraction : 'a list (** per pin: where in the cycle the edge goes *)
      ; mode : 'a list (** per pin, host: {!Mode.to_int} *)
      ; mode_write : 'a list
      (** per pin, host: high for a cycle when the host writes the pin's
          mode, even with an unchanged value; re-arms the pin after a fault *)
      ; settle_cycles : 'a (** host *)
      ; scale : 'a list (** per line, host *)
      ; offset : 'a list (** per line, host *)
      ; ratio : 'a list (** per line, host *)
      ; clear_flags : 'a list
      (** per pin: clears the sticky flags; does not leave the fallback *)
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { out : 'a list (** per pin: to the pad *)
      ; loopback : 'a list (** per pin: the XOR, before the output mux *)
      ; line_out : 'a list (** per line: for the loopback into the capture block *)
      ; active_mode : 'a list
      ; fallback : 'a list (** the pin is in bypass because of a fault *)
      ; overrun : 'a list
      ; range_fault : 'a list
      ; mismatch : 'a list
      ; overrun_event : 'a list
      ; range_event : 'a list
      ; model_toggle : 'a list (** per line, [Model] only *)
      ; model_ticks : 'a list (** per line, [Model] only *)
      }
    [@@deriving hardcaml]
  end

  (** What the control drives into one line. *)
  module Line_select : sig
    type 'a t =
      { launch : 'a (** the launch flop's D *)
      ; coarse : 'a
      ; fine : 'a
      ; coarse_bypass : 'a
      ; fine_bypass : 'a
      }
    [@@deriving hardcaml]
  end

  (** The fraction -> (coarse, fine) mapping (combinational). *)
  module Map : sig
    module I : sig
      type 'a t =
        { fraction : 'a
        ; scale : 'a
        ; offset : 'a
        ; ratio : 'a
        ; mode : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { coarse : 'a
        ; fine : 'a
        ; range_fault : 'a
        }
      [@@deriving hardcaml]
    end

    val create : Signal.t I.t -> Signal.t O.t
  end

  (** The lines, XOR, level flop and output mux of each pin. *)
  module Analog : sig
    type pin =
      { rise : Signal.t Line_select.t
      ; fall : Signal.t Line_select.t
      ; level : Signal.t (** the level flop's D *)
      ; bypass : Signal.t
      }

    type pin_out =
      { out : Signal.t
      ; loopback : Signal.t
      ; line_out : Signal.t * Signal.t (** rise, fall *)
      ; model_toggle : Signal.t * Signal.t
      ; model_ticks : Signal.t * Signal.t
      }

    val create
      :  name:string
      -> implementation
      -> clock:Signal.t
      -> clear:Signal.t
      -> pin list
      -> pin_out list
  end

  (** [create ?name implementation scope i]. [name] (default ["vernier"])
      prefixes instance and register names. *)
  val create : ?name:string -> implementation -> Scope.t -> Signal.t I.t -> Signal.t O.t

  (** As [create], as a separate module; [Model] only in a flattened scope. *)
  val hierarchical
    :  ?name:string
    -> module_name:string
    -> implementation
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end

module Make (_ : Config) : S

(** Host-side calibration: constants from an identity-constant sweep. *)
module Calibration : sig
  (** Delays (ps, launch clock edge to the measured edge) at coarse taps
      [0, 1, ...] in the mode being calibrated (identity constants), and at
      fine taps [0, 1, ...] in fine mode (read in [Full] only). *)
  type sweep =
    { coarse : float array
    ; fine : float array
    }
  [@@deriving sexp_of]

  (** Per line: launches with the launch flop rising and falling. *)
  type line =
    { rising : sweep
    ; falling : sweep
    }
  [@@deriving sexp_of]

  type t =
    { scale : int array
    ; offset : int array
    ; ratio : int array
    ; d_align_ps : float (** the zero-fraction delay every line is aligned to *)
    ; coarse_step_ps : float array (** fitted, per line *)
    ; fine_step_ps : float array
    ; skew_ps : float array (** zero-code delay, rising minus falling launch, per line *)
    }
  [@@deriving sexp_of]

  (** [fit ~coarse_taps ~period_ps ~mode lines] for [Full] or [Coarse]: per
      line, least-squares coarse and fine steps (both launch directions
      averaged) and zero-code delay; [d_align_ps] = the largest zero-code
      delay plus [margin_ps] (default 0), so every offset is non-negative.
      Codes are rounded; raises if one does not fit its field (a scale
      outside [1 .. 2^(coarse_bits + scale_frac_bits) - 1], e.g. a clock
      slower than the scale field covers; an offset outside
      [0 .. 2^offset_bits - 1]; in [Full], a ratio outside
      [0 .. 2^ratio_bits - 1]), since the hardware would use a clipped
      constant without a fault. The host then keeps the pin in bypass. In
      [Coarse] the fine sweeps are not read: [ratio] is 0 and
      [fine_step_ps] NaN. *)
  val fit
    :  ?margin_ps:float
    -> coarse_taps:int
    -> period_ps:float
    -> mode:Mode.t
    -> line array
    -> t
end
