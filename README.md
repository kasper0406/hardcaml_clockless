# hardcaml_clockless

Sub-clock and clockless circuit support for [Hardcaml](https://github.com/janestreet/hardcaml).

Hardcaml describes synchronous logic, and its tool flow (logic synthesis,
static timing) assumes the same. Some structures only work if specific
standard cells are instantiated exactly as written:
- delay lines;
- time-to-digital converters (TDCs) that timestamp edges far below the clock
  period;
- output edge verniers;
- clockless handshake pipelines.

This library builds those structures from cells read out of the PDK's Liberty
file. It protects them from synthesis (`keep`) and generates what
place-and-route needs to leave them alone (dont-touch lists, loop breaks,
island clocks), checks synthesised and routed netlists against them, and runs
delay-aware simulations. Relative-timing checks on the routed netlist are in
progress.

"Hand-placed" below means hand-*instantiated*: the cells and their
connections are fixed, but nothing constrains placement or routing, so wire
delay, load and skew are whatever the placer makes of them. It sits on top of Hardcaml's public API, with no changes to Hardcaml
itself, just as `hardcaml_waveterm` or `hardcaml_circuits` do.

## Status

| Module | What | State |
|---|---|---|
| `Liberty` | Liberty (.lib) parser: groups, attributes, tables | done |
| `Bool_expr` | Liberty boolean functions: parse, evaluate, lower to Hardcaml logic | done |
| `Timing_library` | Liberty timing view: NLDM delay/slew tables per arc (bilinear interpolation, extrapolation), pin capacitances, `ff`/`latch` groups, setup/hold and recovery/removal tables, minimum pulse widths, `wire_load` models and selection | done |
| `Cell_library` | Typed cells: kind (combinational/latch/flop/tristate/constant/physical), pins, functions, area | done |
| `Cell` | Hand-placed cell instantiation with `dont_touch`/`keep` (honoured by yosys, not by OpenROAD: see `Sdc`), pin checking, stable instance and output net names | done |
| `Delay_line` | Tapped chain of buffer/delay cells; multi-cell stages (e.g. dlygate/inv/dlygate/inv, rise/fall balanced), checked non-inverting | done |
| `Ring` | Free-running ring oscillator: NAND enable + inverters, selectable lengths, ring-clocked Johnson+Gray period counters (two copies half a period apart), loop-break metadata. `Clk_sampler` decodes the system-clock-sampled state to ring time `k*2n+p`, choosing the stable counter by phase. `Ideal` is a unit-delay reference ring in plain OCaml that drives the behavioural `Model` | done. Verified in Cyclesim, and in `Event_sim` with Liberty delays at three corners, pre-layout with the Liberty 1k wire load: every sample decodes to the true ring time, no pattern errors; each counter is read in a window centred in its stable interval, measured from the trace with the sampler's setup/hold aperture (>= 5.6 stages of margin on both sides for a fixed 17-stage ring, asserted >= 4). The window ends `L`/`E` depend on the ring's load and are measured per ring: `L = 12`, `E = 6` for a bare ring, `L = 9`, `E = 4` when it also drives Pin_capture. Fixed 17-stage ring: 19-203 ps bins and 29-83 ps rms error on held-out calibrated intervals (flat 1 fF wires: 19-132 ps, 27-73 ps). Use a fixed length (the length-select mux widens bins ~4x) |
| `Tdc` | Delay line + first-stage sampling flops (kept registers, or flop cells with stable instance names) + synchronous decoder: bubble filter, fine code, overflow and multi-edge flags, coarse counter. `Cells` (Verilog) or behavioural `Model` (Cyclesim). Fallback design: a line of buf_1 does not span a 20 ns cycle at the fast corner | done |
| `Pin_capture` | XOR-merged input pins clock a dual-edge asynchronous FIFO: two banks (merged rising/falling) of flop-cell slots loaded through enable muxes (clock never gated), Johnson write pointers; each record holds ring nodes, counters A/B, the pin snapshot, a delayed snapshot and a tag (the write pointer, loaded through a lagged select). A synchronous drain reads banks in turn, attributes pins by snapshot diff, decodes ring time (`Ring.S.decode`) and flags everything it cannot vouch for: torn records (two writes to one bank closer than the slot-select handoff; the tag does not match), times outside the capture latency before the read (against the clk sampler's time), lost edges (per-pin Gray edge counters), level mismatches, stale and unbalanced attributions (edges that cancelled in the XOR), overwrites, runts, invalid pointers (auto-reset), enable changes. After a resync that discarded captures the next event carries `lost` (D-13); levels re-baseline silently. `Model` + `Reference`/`Model_driver` for Cyclesim | done. Event_sim with Liberty delays at typ/slow/fast, pre-layout with the Liberty 1k wire load (flat 1 fF wires in brackets), a 4-stage dlygate4sd3 recent-edge delay (the physical-flow gate: 2 lose to the routed bank clocks), 105k random edges on 4 pins (10% within 2 ns of the previous edge), checked against the true edges with bounded flag deadlines and fault windows (`test/capture_oracle.ml`): no unflagged error at any corner (nor with flat wires), every resync's next event marked `lost`; 87-89% of edges validated in time (88-90%); torn-record flags ~1 per 270-1500 edges (slow-fast), nearly all conservative (the record before a too-close second write); the plausibility check never fired alone (0 false flags; ages of unflagged events 1.0-6.1 cycles against a 1-12 window); record tags lag the other bits by >= 152 ps over setup + hold at fast (>= 42 ps with 3% variation and 30 ps clock skew); ring period 1.69/2.61/4.19 ns fast/typ/slow (1.07/1.60/2.54); counters read with `L = 9`, `E = 4`, measured for the capture slots and the clk sampler (>= 6.2 stages of margin, asserted >= 4); calibrated single-edge error (calibration frozen from a separate run) 0.6 bin rms, 30/47/73 ps fast/typ/slow (0.8 bin, 23/37/60 ps), p99 72/117/183 ps, rare outliers beyond 3 bins (1 in 90k, 0.43 ns); snapshot race margin >= 280 ps (190); two-pin collisions merge (attributed to both) below 350/575/900 ps (250/350/600), cancel (reported untimed, flagged) below ~200-475 ps (125-300); bursts of three edges within ~1 ns cause flagged capture faults (~1 per 700-1600 edges); edge-to-event latency: the event is valid after the 5th rising clock edge at or after the pin edge (median; 5-6 for an isolated edge, 9 while the drain re-aligns after a runt; p99 7, max 10 under random load). Area (yosys, 4 pins, ring excluded): 48.7k µm² at 2 slots/bank, 56.8k at 3 (39.9k and 47.3k before the record tags and plausibility check; the front's share of the increase is ~0.9k at 2 slots, the rest is the drain's tag compare and time check). Slots per bank are a parameter (2–4); `capture_gate.exe` takes the slot count as its fifth argument |
| `Vernier` | Output edge vernier (bitbanger D-3): per pin a rise and a fall line (launch flop, coarse chain of balanced dlygate4sd3/inv stages + equal-depth mux2 tree, fine buf_1 chain + tree, section bypass muxes), XOR recombiner, level flop and a bypass mux last before the pin. Modes bypass / coarse / fine (diagnostic, direct tap) / full; per-line host constants (scale, offset, fine ratio; scale 0 = identity for tap sweeps), one fraction -> tap mapper per pin; selects change only on quiet lines (`settle_cycles`), unchanged selects may launch every cycle; overruns and range faults drop the pin to bypass, sticky flags, and only a mode write re-arms it; loopback per line; `Calibration.fit` (host); behavioural `Model` | done. Event_sim at typ/slow/fast, "1k" wire load: tap delays equal the closed-form Liberty oracle to 25 fs, monotonic, coarse span 21.9 ns at fast (>= 20 ns), fine span >= one coarse step at every corner; line latency 56.8 ns at slow (`settle_cycles` 3, 4 with margin: same-direction edges with a new select >= 4-5 cycles apart). With 3% variation and clock skew, calibrated error 31-79 ps rms, 110-240 ps worst (0.4-1.0 / 1.4-3.1 LSB of 78 ps); 2.1k random edges, pulses >= 1 ns, bursts and mode switches: edges = requests, no runts, no pulse-width violations; with a stuck coarse cell, fine mode and bypass still exact, stuck fine cell: coarse mode, stuck XOR: bypass; without the settle rule the line glitches. Area (yosys, 2 pins): 34.0k µm² (hand-placed 11.8k, mappers 2 x 6.0k), over the 25k budget |
| `Strobe_sampler` | Vernier-delayed strobe samples every pin: equivalent-time sampling for repetitive signals | planned |
| Calibration | Code-density histogram of the `clk`-sampled ring; ring periods per cycle | next |
| Behavioural models | Cyclesim models so the rest of a design simulates normally | `Tdc.Model`, `Ring.Model` (driven by `Ring.Ideal`), `Pin_capture.Model` (driven by `Pin_capture.Reference` / `Model_driver`) |
| `Event_sim` | Event-driven gate-level simulation of Hardcaml circuits of Liberty cells: fs time, 0/1/X, NLDM delays with slew propagation and loads, inertial/transport, flops/latches from Liberty with setup/hold-aperture metastability (exponential resolution, optionally corner-scaled tau), corner scale and seeded per-instance variation (with per-instance overrides), Hardcaml registers on any clock net, loops (ring oscillators), VCD. Opt-in realism: Liberty `wire_load` (or per-fanout) wire capacitance and Hardcaml input loads, a report of untimed Hardcaml logic next to cells (warn/fail), minimum pulse width and recovery/removal checks (log, or log and go metastable), clock insertion delay with seeded per-pin skew, Hardcaml registers that resolve an X they load to a seeded random bit. `Config.pre_layout`: the Liberty wire load, both checks logged, registers resolving X | done (minimal; defaults are pre-layout-optimistic: flat 1 fF wires, ideal clocks, no checks; the ring and Pin_capture numbers above use `Config.pre_layout`); SDF, wire RC, adversarial delays, hazard/protocol monitors, Cyclesim co-simulation: planned |
| `Sdc` | From the flattened circuit: protected instances (every `dont_touch` cell and kept register) and nets (driven by them, plus registered fork nets); LibreLane `RSZ_DONT_TOUCH_RX`, or `RSZ_DONT_TOUCH_LIST` resolved from a synthesised netlist (it names the flops yosys made of kept registers); SDC fragments: `set_disable_timing` at loop breaks, false paths into the TDC line, and for signoff only `create_clock` on ring counter, Pin_capture bank and edge-counter clocks with asynchronous groups (CTS ignores dont-touch, so they stay out of the PnR SDC) | done (generation); `Sdc.vernier` cuts the lines from registers and ports and bounds the select and drain paths (`set_max_delay` to internal pins; checked with OpenSTA by `test/yosys/sta_vernier.sh`). Not yet run through LibreLane/OpenROAD. Relative-timing checks (snapshot race, hold, pulse width, reset recovery at 3 corners): planned |
| Post-route check | `Netlist` parses gate-level Verilog (yosys, OpenROAD); `Sdc.check` re-extracts the islands: same cells, protected nets unbuffered, unrenamed, still driving their outputs and without extra loads (antenna diodes allowed; loads from synthesised logic are not checked one by one), kept registers still plain D flops with their D, clock edge and source, reset tied off, inputs rebuffered only from their named source, constants possibly tie cells | done; checked on yosys output (`test/yosys/yosys_island.sh`), not yet on a routed netlist |
| `Click`, `Bd_channel` | Bundled-data click handshake components with a synchronous reference | planned |
| Workcraft export | Formal checks: deadlock freedom, hazard freedom, conformation | planned |

## Event-driven simulation

```ocaml
let lib = Timing_library.load "sg13cmos5l_stdcell_typ_1p20V_25C.lib" in
(* build a circuit with Cell.instantiate lib.library ..., close loops with Signal.wire *)
let sim = Event_sim.create lib circuit in
Event_sim.set_input sim "en" V1 ~at:(Event_sim.of_ns 5.);
Event_sim.add_clock sim "clk" ~period:(Event_sim.of_ns 20.);
Event_sim.on_change sim "ring_s8/Y" (fun ~time v -> ...);
Event_sim.run_until sim (Event_sim.of_ns 10_000.);
```

Nets are named by port, by signal name (`s[i]` for bus bits) and by cell pin
(`<instance>/<pin>`). Semantics are documented in `src/event_sim.mli`.

Measured at typ (1.2 V, 25 °C), a NAND2 + 8 × inv_1 ring has a 550.6 ps period,
equal to the period computed independently from the tables. Against the real
corner libraries (`test/data/*_excerpt_{slow,fast}*.lib`) it is ×1.58 (slow,
1.08 V 125 °C) and ×0.69 (fast, 1.32 V −40 °C). The global `corner` factor that
reproduces these is about 1.40 and 0.71 (slews scaled too, the default), not the rough
×2.1/×0.48 estimate; prefer loading the real corner file. A NAND2 +
4 × (dlygate4sd1_1, inv_1) ring (1.47 ns) sampled by 9 flops on 50 MHz plus a
ring-clocked 16-bit counter simulates 10 µs in about 0.1 s
(`dune exec bench/ring_bench.exe`).

The simulator figures in this section use the defaults: a flat 1 fF of wire per net, untimed
Hardcaml logic and ideal clocks. With the library's default `wire_load`
("1k": about 2 fF at fanout 1, 12 fF at fanout 5;
`{ Config.default with wire_load = Liberty_default }`) the 9-stage ring is
623.8 ps (+13%). The preset `Config.pre_layout` (that wire load, the minimum
pulse width and recovery/removal checks logged, Hardcaml registers resolving
an X they load) is the baseline for the reported ring and Pin_capture
numbers (`test/test_ring_timing.ml`, `test/test_pin_capture_timing.ml`), with
flat wires kept as regressions. For less optimistic numbers also set `prim_input_cap`,
`clock_insertion`/`clock_skew_sigma`, `variation_sigma`, the
`pulse_width_check`/`recovery_removal_check` options, and check
`Event_sim.untimed_logic` (see `src/event_sim.mli`).

## Physical flow

```ocaml
let sdc =
  Sdc.create circuit (* flattened; ~prefix:"island." if yosys flattens it *)
  |> Sdc.ring ~name:"ring" ~loop_breaks ~period_ns:2.3 (* see below *)
  |> Sdc.pin_capture ~name:"pin_capture" ~pins:4 ~min_edge_spacing_ns:2.0
  |> Sdc.tdc ~name:"tdc"
  |> Sdc.vernier ~name:"vernier" ~pins:2 ~period_ns:20.0 ~settle_cycles:3 (* smallest the host loads *)
in
Sdc.sdc sdc ~stage:Pnr ~system_clock:"clk" (* append to PNR_SDC_FILE *)
Sdc.sdc sdc ~stage:Signoff ~system_clock:"clk" (* append to SIGNOFF_SDC_FILE *)
(* library: a Timing_library.t; synthesised: LibreLane's Yosys.Synthesis netlist *)
let names = Or_error.ok_exn (Sdc.dont_touch_list sdc ~library synthesised) in
Sdc.dont_touch_rx names (* RSZ_DONT_TOUCH_RX (not RSZ_DONT_TOUCH_LIST) *)
Sdc.fanout_violations sdc ~library ~max_fanout:10 synthesised (* nets OpenROAD will not repair *)
Sdc.check sdc ~library ~dont_touch:names ~reference:synthesised routed (* [] if the islands survived *)
Sdc.ring_placement sdc ~name:"ring" ~library ~rows ~site ~row ~columns:2 ~row_pitch:11 ~column_gap:50
(* MACRO_PLACEMENT_CFG: the ring and its node loads, fixed before placement *)
```

LibreLane applies the dont-touch settings only in its resizer steps, so
island clocks are left out of the PnR SDC (clock tree synthesis would buffer
them) and only appear at signoff. Protected are the island's cells and its
nets, including *boundary* nets from synthesised logic into a cell (OpenROAD
stops with RSZ-3006 when it tries to buffer a net with a dont-touch load), but
not cell outputs that only feed synthesised logic (hold repair must reach
them). Since OpenROAD repairs no protected net, Ring and Vernier take every
control into their cells through an isolating buffer (`Cell.isolate`: kept by
synthesis, repairable by OpenROAD) and a cell buffer tree within 8 loads
(`Cell.fanout`). The names are resolved from the synthesised netlist and given to
LibreLane as one exact-match regex: its list variable loses bus bits. One
signoff SDC serves all corners, and the ring's counters are timed against
its `period_ns`: give the typical corner's period (LibreLane checks
violations there) and redo each corner with its own (below). See
`src/sdc.ml`.

The physical-flow gate (issue #7) runs a stub island, `test/flow/gen_stub.ml`
(the 17-stage ring with its sampler, a 4-pin Pin_capture and the chip's
2-pin Vernier, with serial configuration and readout), next to
janestreet-bitbanger's chip through the Tiny Tapeout IHP flow pinned to the
GDS action (LibreLane 3.1.0.dev3, locally in Docker: that repository's
`flow/`). `test/flow/sta_island.tcl` and `test/flow/island_timing.py` give
the three-corner signoff of the routed island and an SDF-based comparison of
its ring, capture races and vernier lines before and after layout.
Result (2026-10-29, after the follow-ups): every protected cell and net
survives every OpenROAD step unchanged (antenna diodes on a captured pin
aside); no max-capacitance violation (the Vernier clear behind its isolating
buffer); Pin_capture's pointer reset (18 loads) and its select and clock
leaf buffers (11-13) remain above the max fanout. Unplaced, the ring's nodes
carried 2-3.7 times the "1k" wire capacitance and its period varied with the
placer (typical 3.7-5.9 ns over four runs); as a compact block, a fifth of
it, and a period 40% shorter than characterised, which broke the counter
selection margins (0-1 stage) and the counters' one-period settling. At an
11-row pitch the nodes carry 0.93 of "1k" and the period is 2.42-2.71 ns
typical in both runs (2.53-2.79 before layout), with counter selection
margins >= 4.2 stages, record tags lagging by >= 62 ps over setup and hold,
and the recent-edge delay (four dlygate4sd3) and snapshot race margins at
>= 351 and >= 260 ps at all three corners. Details in janestreet-bitbanger's
`flow/README.md`.

## Testing

Tests never use the library's own output as the expectation where an
independent oracle exists. Cell functions parsed from Liberty are checked
exhaustively against hand-written reference semantics (`test/`), and the
roadmap applies the same rule throughout:
- randomised and adversarial delays;
- mutants that must be caught;
- comparison against synchronous references.

```sh
opam repo add janestreet-bleeding https://github.com/janestreet/opam-repository.git
opam install . --deps-only --with-test
dune runtest
```

`dune exec test/gate/capture_gate.exe -- EDGES_PER_CORNER [SEED [NEAR [MIN_SAME_PIN_NS [SLOTS [WIRES]]]]]`
is the long Pin_capture gate run (random edges on 4 pins at three corners,
checked against the true edges with the oracle's flag deadlines and fault
windows; it exits 1 on any unflagged error or a resync whose next event
lacks the `lost` mark; `WIRES` is `1k`, the default, or `flat`); `test/yosys/yosys_capture.sh [SLOTS]`
synthesises Pin_capture and reports its area and that every hand-placed
cell survived.

`test/yosys/yosys_island.sh` synthesises a stub island (ring, sampler,
Pin_capture, TDC) with the passes LibreLane's synthesis runs and checks the
netlist with `Sdc.check`.

`test/yosys/yosys_vernier.sh` synthesises the chip's 2-pin Vernier and one
mapper, reports their area and checks every hand-placed cell survived.
`test/yosys/sta_vernier.sh` runs OpenSTA on it with `Sdc.vernier`'s
constraints: the select and drain bounds, and that the lines are cut from
the pin while the bypass path stays timed.

`test/yosys/yosys_tdc.sh` is a synthesis smoke check outside `dune runtest`
(it needs yosys and the full SG13CMOS5L liberty): it synthesises a TDC with
yosys + abc and checks that every delay cell and sampling flop survives.

Test data in `test/data/` is an excerpt of the IHP Open PDK Liberty file
(Apache-2.0).

## License

MIT, like Hardcaml.
