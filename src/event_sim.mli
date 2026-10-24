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
    or the slew given to {!set_input}). The load of a net is
    [Config.wire_cap] plus the [capacitance] of every cell input pin on it.
    Each delay is multiplied by [Config.corner] and by the instance's
    variation factor, then rounded to 1 fs; output slews are scaled the same
    way when [Config.corner_scales_slew]. With [Zero] or [Unit d] every cell
    has that delay and timing checks are off.

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
    decision waits until the latest hold time has passed). Every metastable
    capture is logged ({!metastable_events}). This is deliberately
    pessimistic: any aperture violation goes metastable. Recovery/removal and
    minimum pulse width are not checked.

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

  type t =
    { delay_mode : delay_mode
    ; pulse_mode : pulse_mode (** for combinational cells and primitives *)
    ; corner : float (** global delay scale; ~1.40 slow, ~0.71 fast (see README) *)
    ; corner_scales_slew : bool
    ; variation_sigma : float
    (** per-instance factor [max 0.05 (1 + sigma * N(0,1))], seeded, drawn in
        instance-name order *)
    ; seed : int
    ; wire_cap : float (** pF per net *)
    ; input_slew : float (** ns, for primary inputs *)
    ; prim_delay : int (** fs, Hardcaml combinational primitives *)
    ; reg_clock_to_q : int (** fs, Hardcaml registers *)
    ; metastability : bool (** off: liberty flops sample ideally at the edge *)
    ; tau : float (** ns, mean metastability resolution time *)
    ; flop_init : init
    ; reg_init : init (** Hardcaml registers without [initialize_to] *)
    }
  [@@deriving sexp_of]

  (** Liberty delays, inertial, corner 1, no variation, seed 0, 1 fF wire,
      50 ps input slew, zero-delay primitives and registers, metastability on
      with tau = 30 ps, flops start X, registers start 0. *)
  val default : t
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

(** Variation factor of every cell instance. *)
val instance_factors : t -> (string * float) list

val events_processed : t -> int
val num_nets : t -> int
val config : t -> Config.t

(** {2 Waveforms} *)

(** Record every change from now on (memory grows with the run). *)
val start_tracing : t -> unit

(** Write the recorded changes of all named nets (one name per net) as VCD,
    timescale 1 fs. *)
val write_vcd : t -> Stdio.Out_channel.t -> unit
