(** Event-driven, delay-aware gate-level simulation.

    Cyclesim cannot simulate hand-placed cells, loops through them, several
    clocks or anything below a clock cycle. [Event_sim] simulates a Hardcaml
    [Circuit.t] built from Liberty cells (see {!Cell}) as a netlist of timed
    events, so ring oscillators, ring-clocked counters and flops sampling
    asynchronous edges behave as on silicon, to the accuracy of the Liberty
    tables.

    {2 Time and values}

    Time is an [int] in femtoseconds from 0 ({!of_ns}, {!of_ps}). Values are
    0, 1 and X; every net starts at X. A tristate output that is disabled
    reads X (there is no bus resolution or charge retention).

    {2 Netlist}

    The circuit is bit-blasted. Supported:
    - [Inst] nodes naming a cell of the timing library, with 1-bit pins: any
      kind (combinational, tie, tristate, [ff], [latch]; physical cells have no
      outputs and are ignored);
    - wires (loops through cells are fine: close a ring with [Signal.wire] and
      [Signal.assign]), constants, selects and concatenations (these only
      rename bits);
    - [Not], [Op2] (bitwise ops 3-valued per bit; arithmetic and comparisons
      X if any input bit is X), [Mux] and [Cases], with delay
      [Config.prim_delay];
    - [Reg] with any 1-bit clock net and edge, async [reset] (level), sync
      [clear], [enable] and [initialize_to], with delay
      [Config.reg_clock_to_q] and no timing checks.

    Hardcaml primitives and registers are untimed: a fixed delay (zero by
    default) whatever their load, and each input bit loads its net with only
    [Config.prim_input_cap]. {!untimed_logic} lists those connected to Liberty
    cells; [Config.untimed_logic] can warn about them or reject the circuit.

    Rejected with {!Unsupported}: memories, instantiations of anything that is
    not a library cell (hierarchical designs must be flattened), multi-bit
    cell pins, undriven non-input wires, nets with several drivers.

    {2 Naming}

    Nets are found by name: circuit ports and named signals ([--]); bits of a
    multi-bit signal [s] as [s[i]] (and the whole bus as [s] in {!values}
    and {!to_int}); and every cell pin as ["<instance>/<pin>"], e.g.
    ["ring_s3/Y"] or ["smp0/D"], so stable instance names are enough to find
    every node of a hand-placed structure.

    {2 Delays}

    In [Liberty] mode a cell output change caused by an input pin uses the
    timing arc from that pin (the one whose [when] condition holds, else one
    without [when]); [cell_rise]/[cell_fall] for the delay and
    [rise_transition]/[fall_transition] for the output slew, by bilinear
    interpolation in (input slew, load), linearly extrapolated outside the
    table. Going to X uses the smaller of the two. The input slew is the slew
    the input net was last driven with (primary inputs: [Config.input_slew]
    or the slew given to {!set_input}). Each delay is multiplied by
    [Config.corner] and by the instance's variation factor, then rounded to
    1 fs; output slews are scaled the same way when
    [Config.corner_scales_slew]. With [Zero] or [Unit d] every cell has that
    delay and timing checks are off.

    The load of a net is the [capacitance] of every cell input pin on it,
    [Config.prim_input_cap] per Hardcaml primitive/register input bit on it,
    and a wire capacitance set by [Config.wire_load]: [Flat] is
    [Config.wire_cap] on every net; [Per_fanout c] adds [c] per load pin; the
    [Liberty_*] models replace [wire_cap] by the Liberty [wire_load] model's
    capacitance at the net's fanout (load pins: cell inputs and Hardcaml input
    bits; output ports do not count), as OpenSTA estimates it (see
    {!Timing_library.Wire_load}). Wire resistance is ignored.

    Combinational outputs use inertial delay by default: a pending output
    change is cancelled if the output's newly evaluated value differs from it
    before it lands, so input pulses shorter than the gate delay are
    swallowed. [Transport] instead keeps every change and drops only pending
    changes scheduled at or after a new one. Flop and latch outputs always
    use transport.

    {2 Flops, latches and metastability}

    [ff] and [latch] groups are evaluated from their Liberty expressions
    ([clocked_on], [next_state], [enable], [data_in], [clear], [preset]).
    Each data pin with [setup_*]/[hold_*] arcs against the clock pin has, for
    each transition, an aperture (edge - setup, edge + hold) from the
    [rise_constraint]/[fall_constraint] tables at (data slew, clock slew). A
    transition inside it makes the internal state X; the state resolves to a
    uniformly random 0/1 after a delay drawn from an exponential with mean
    [Config.tau], so Q is X from edge + clk->Q to resolution + clk->Q.
    Transitions before their aperture are captured, later ones are not (the
    decision waits until the latest hold time has passed; a resolution that
    would fall before that decision is clamped to it, so simulation time
    never runs backwards). Every metastable
    capture is logged ({!metastable_events}). This is deliberately
    pessimistic: any aperture violation goes metastable. With
    [Config.tau_scales_with_corner], tau is multiplied by [Config.corner] and
    the instance's factor (tau is set by the latch pair's C/gm, which scales
    roughly like a gate delay); with a real corner library ([corner = 1])
    scale [Config.tau] yourself, e.g. by the ratio of the flop's clk->Q
    delays. Resolution is timed from the clock edge, not from D arrival, and
    Q never shows a runt.

    {2 Pulse width and recovery/removal checks}

    Off by default ([Ignore]); [Log] records violations
    ({!timing_violations}) without changing the simulation;
    [Log_and_metastable] also makes the flop/latch state X and resolves it as
    for a metastable capture, starting when the violation is detected
    (whatever [Config.metastability]). Liberty delay mode only.

    [Config.pulse_width_check]: when an input pin with minimum pulse widths
    (see {!Timing_library.Pulse_width}; or, with [Config.min_pulse_width], any
    clock/enable, clear or preset pin of a flop or latch without them, both
    levels) ends a pulse shorter than the minimum at the slew of its leading
    edge, times [corner] and the instance factor. A pulse is checked only if
    it began with a 0/1 transition.

    [Config.recovery_removal_check]: [recovery_*]/[removal_*] arcs of an
    asynchronous pin against the clock give, like setup/hold, a window
    (edge - recovery, edge + removal) for each transition of the pin
    ([rise_constraint] for rising transitions, e.g. [RESET_B] deasserting) at
    (pin slew, clock slew); a transition strictly inside the window of an
    active clock edge is a violation, whether or not the reset was still
    active at the edge. Transitions are checked at the edge against the pin's
    recent history and afterwards against the last edge only.

    {2 Clock insertion delay and skew}

    With [Config.clock_insertion] or [Config.clock_skew_sigma] positive, the
    clock (or enable) pin of every Liberty flop and latch sees its net
    delayed by [max 0 (insertion + sigma * N(0,1))] (static skew, not
    jitter; keep insertion well above sigma or the clipping at 0 biases it),
    drawn per instance in instance-name order from its own seeded stream;
    [Config.clock_delay_overrides] sets it per instance. The pin then reads a
    private, transport-delayed copy of the net (named ["<instance>/<pin>"]),
    its capacitance still loads the original net, and setup/hold, metastable
    edge times and checks refer to the delayed edge ({!clock_delays}).
    Hardcaml registers see their clock undelayed.

    A clock (or latch enable) changing from 0 to X, while it is X, makes the
    state X where the data differs from it; a net resolving from its
    power-up X is not an edge. Liberty flops start at X ([Config.flop_init]),
    Hardcaml registers at [initialize_to] or 0 ([Config.reg_init]).

    Same-time events run in the order they were scheduled; zero-delay
    outputs land after everything already scheduled for that time, so
    registers sample their D before a same-edge update reaches it. A loop
    with zero total delay (e.g. a ring in [Zero] mode) raises after a million
    events at one instant instead of hanging. *)

open! Base
open Hardcaml

module Value : sig
  type t =
    | V0
    | V1
    | X
  [@@deriving sexp_of, equal, compare]

  val to_char : t -> char
  val of_bool : bool -> t
end

(** {2 Time} *)

val fs_per_ns : int
val of_ns : float -> int
val of_ps : float -> int
val to_ns : int -> float
val to_ps : int -> float

module Config : sig
  type delay_mode =
    | Liberty
    | Zero
    | Unit of int (** fs per cell *)
  [@@deriving sexp_of]

  type pulse_mode =
    | Inertial
    | Transport
  [@@deriving sexp_of]

  type init =
    | Init_x
    | Init_zero
  [@@deriving sexp_of]

  type wire_load =
    | Flat (** [wire_cap] on every net *)
    | Per_fanout of float (** [wire_cap] plus this many pF per load pin *)
    | Liberty_default (** the library's [default_wire_load] *)
    | Liberty_named of string (** a [wire_load] group by name *)
    | Liberty_by_area of float option
    (** the library's [default_wire_load_selection] for this design area
        (library area units; [None]: the circuit's total cell area) *)
  [@@deriving sexp_of]

  type untimed_logic =
    | Allow
    | Warn (** print {!untimed_logic} to stderr in {!create} *)
    | Fail (** {!create} raises {!Unsupported} if {!untimed_logic} is not empty *)
  [@@deriving sexp_of]

  type check =
    | Ignore
    | Log (** record in {!timing_violations} *)
    | Log_and_metastable (** and make the flop/latch state metastable *)
  [@@deriving sexp_of]

  type t =
    { delay_mode : delay_mode
    ; pulse_mode : pulse_mode (** for combinational cells and primitives *)
    ; corner : float (** global delay scale; ~1.40 slow, ~0.71 fast (see README) *)
    ; corner_scales_slew : bool
    ; variation_sigma : float
    (** per-instance factor [max 0.05 (1 + sigma * N(0,1))], seeded, drawn in
        instance-name order *)
    ; seed : int
    ; wire_cap : float (** pF per net ([Flat] and [Per_fanout]) *)
    ; wire_load : wire_load
    ; prim_input_cap : float (** pF per Hardcaml primitive/register input bit *)
    ; input_slew : float (** ns, for primary inputs *)
    ; prim_delay : int (** fs, Hardcaml combinational primitives *)
    ; reg_clock_to_q : int (** fs, Hardcaml registers *)
    ; reg_resolves_x : bool
    (** a Hardcaml register that loads X on a clean clock edge loads a
        seeded random 0 or 1 instead (counted, {!reg_x_resolutions}): a
        real flop resolves. For registers that may sample a cell output
        while it is metastable or changing (e.g. a read racing an
        asynchronous write), so that the X does not stand for a value
        forever. Off by default: it also hides X from missing
        initialisation. *)
    ; untimed_logic : untimed_logic (** Liberty mode only *)
    ; metastability : bool (** off: liberty flops sample ideally at the edge *)
    ; tau : float (** ns, mean metastability resolution time *)
    ; tau_scales_with_corner : bool (** tau times [corner] and instance factor *)
    ; pulse_width_check : check
    ; min_pulse_width : float option
    (** ns, both levels, for flop/latch clock and async pins without Liberty data *)
    ; recovery_removal_check : check
    ; clock_insertion : float (** ns, mean delay to every flop/latch clock pin *)
    ; clock_skew_sigma : float (** ns, per-pin spread around [clock_insertion] *)
    ; factor_overrides : (string * float) list
    (** instance -> variation factor, replacing the drawn one (the draws are
        still made, so other instances keep theirs) *)
    ; clock_delay_overrides : (string * float) list
    (** flop/latch instance -> clock insertion delay (ns), replacing the drawn one *)
    ; flop_init : init
    ; reg_init : init (** Hardcaml registers without [initialize_to] *)
    }
  [@@deriving sexp_of]

  (** Liberty delays, inertial, corner 1, no variation, seed 0, [Flat] 1 fF
      wire, Hardcaml inputs free, 50 ps input slew, zero-delay primitives and
      registers allowed (not resolving X), metastability on with tau = 30 ps (not scaled),
      pulse width and recovery/removal checks off, ideal clocks, flops start
      X, registers start 0. *)
  val default : t

  (** The pre-layout baseline for the reported ring and Pin_capture numbers
      (owner decision D-9): [default] with the library's default wire load
      ([Liberty_default], "1k" in sg13cmos5l), the minimum pulse width and
      recovery/removal checks set to [Log] (they only record:
      {!timing_violations}), and Hardcaml registers resolving X
      ([reg_resolves_x]). Override [wire_load = Flat] for the flat-wire
      regressions. *)
  val pre_layout : t
end

module Metastable_event : sig
  type t =
    { instance : string
    ; edge_time : int
    ; resolve_time : int (** internal state resolves; Q follows after clk->Q *)
    ; resolved_to : Value.t
    }
  [@@deriving sexp_of]
end

module Timing_violation : sig
  type kind =
    | Pulse_width of
        { level : Value.t (** the level held too briefly *)
        ; width : int
        ; min_width : int
        }
    | Recovery_removal of
        { edge_time : int (** the clock edge (at the pin) *)
        ; window : int * int (** relative to the edge; the transition was inside *)
        }
  [@@deriving sexp_of]

  type t =
    { instance : string
    ; pin : string
    ; time : int (** of the transition: end of the pulse, or the async pin's *)
    ; kind : kind
    }
  [@@deriving sexp_of]
end

(** A Hardcaml primitive or register connected to Liberty cells: reachable
    from a cell output ([from_cell]) and/or reaching a cell input
    ([to_cell]) through Hardcaml logic only (the first cell found, in
    instance-name order). *)
module Untimed : sig
  type t =
    { element : string (** [reg], [mux], [cases], [not], or the [Op2] ([xor], [add], ...) *)
    ; signal : string (** its names, or [<unnamed uid>] *)
    ; from_cell : string option
    ; to_cell : string option
    }
  [@@deriving sexp_of]
end

exception Unsupported of Sexp.t

type t

val create : ?config:Config.t -> Timing_library.t -> Circuit.t -> t

(** {2 Running} *)

val now : t -> int

(** Process every event up to and including [time]; [now] becomes [time]. *)
val run_until : t -> int -> unit

val run_for : t -> int -> unit

(** Run until no events are pending (never, with a clock) or [limit]; true if
    quiescent. *)
val run_until_quiescent : ?limit:int -> t -> bool

(** {2 Stimulus} (primary inputs only; times are absolute, not in the past) *)

val set_input : ?slew:float -> t -> string -> Value.t -> at:int -> unit

(** All bits of an input bus, from an integer. *)
val set_input_int : ?slew:float -> t -> string -> int -> at:int -> unit

(** A free-running clock on an input: low from now, rising at [start]
    (default 0) and every [period] after, high for [high] (default half). *)
val add_clock : ?start:int -> ?high:int -> ?slew:float -> t -> string -> period:int -> unit

(** {2 Observation} *)

val has_net : t -> string -> bool
val names : t -> string list
val value : t -> string -> Value.t

(** Bits of a bus (lsb first), or of a single net. *)
val values : t -> string -> Value.t array

(** A bus as an integer; [None] if any bit is X. *)
val to_int : t -> string -> int option

(** Slew (ns) the net was last driven with, and its load (pF). *)
val slew_ns : t -> string -> float

val load_pf : t -> string -> float

(** Called on every change of the net, at the time it happens. *)
val on_change : t -> string -> (time:int -> Value.t -> unit) -> unit

(** Oldest first. *)
val metastable_events : t -> Metastable_event.t list

(** Hardcaml register bits that loaded X and were resolved
    ([Config.reg_resolves_x]). *)
val reg_x_resolutions : t -> int

(** Oldest first (by detection). *)
val timing_violations : t -> Timing_violation.t list

(** Hardcaml primitives and registers connected to Liberty cells, whatever
    the delay mode; in Liberty mode these are the untimed parts of a timed
    netlist. *)
val untimed_logic : t -> Untimed.t list

(** Variation factor of every cell instance. *)
val instance_factors : t -> (string * float) list

(** Clock insertion delay (fs) of every flop and latch instance. *)
val clock_delays : t -> (string * int) list

val events_processed : t -> int
val num_nets : t -> int
val config : t -> Config.t

(** {2 Waveforms} *)

(** Record every change from now on (memory grows with the run). *)
val start_tracing : t -> unit

(** Write the recorded changes of all named nets (one name per net) as VCD,
    timescale 1 fs. *)
val write_vcd : t -> Stdio.Out_channel.t -> unit
