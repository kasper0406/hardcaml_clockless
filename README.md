# hardcaml_clockless

Sub-clock and clockless circuit support for [Hardcaml](https://github.com/janestreet/hardcaml).

Hardcaml describes synchronous logic, and its tool flow (logic synthesis,
static timing) assumes the same. Some structures only work if specific
standard cells are placed exactly as written:
- delay lines;
- time-to-digital converters (TDCs) that timestamp edges far below the clock
  period;
- output edge verniers;
- clockless handshake pipelines.

This library builds those structures from cells read out of the PDK's Liberty
file. It protects them from synthesis and place-and-route, and (in progress)
generates the timing constraints and delay-aware simulations needed to verify
them. It sits on top of Hardcaml's public API, with no changes to Hardcaml
itself, just as `hardcaml_waveterm` or `hardcaml_circuits` do.

## Status

| Module | What | State |
|---|---|---|
| `Liberty` | Liberty (.lib) parser: groups, attributes, tables | done |
| `Bool_expr` | Liberty boolean functions: parse, evaluate, lower to Hardcaml logic | done |
| `Timing_library` | Liberty timing view: NLDM delay/slew tables per arc (bilinear interpolation, extrapolation), pin capacitances, `ff`/`latch` groups, setup/hold tables | done |
| `Cell_library` | Typed cells: kind (combinational/latch/flop/tristate/constant/physical), pins, functions, area | done |
| `Cell` | Hand-placed cell instantiation with `dont_touch`/`keep`, pin checking, stable instance names | done |
| `Delay_line` | Tapped chain of buffer/delay cells | done |
| `Ring` | Free-running ring oscillator: NAND enable + inverters, selectable lengths, ring-clocked Johnson+Gray period counters (two copies half a period apart), loop-break metadata. `Clk_sampler` decodes the system-clock-sampled state to ring time `k*2n+p`, choosing the stable counter by phase. `Ideal` is a unit-delay reference ring in plain OCaml that drives the behavioural `Model` | done. Verified in Cyclesim, and in `Event_sim` with Liberty delays at three corners: exact median per-cycle time, no pattern errors; fixed 17-stage ring gives 16-114 ps bins and 27-72 ps rms error on calibrated 20 ns intervals. Use a fixed length (the length-select mux widens bins ~4x) |
| `Tdc` | Delay line + first-stage sampling flops (kept, stably named) + synchronous decoder: bubble filter, fine code, overflow and multi-edge flags, coarse counter. `Cells` (Verilog) or behavioural `Model` (Cyclesim). Fallback design: a line of buf_1 does not span a 20 ns cycle at the fast corner | done |
| `Pin_capture` | XOR-merged input pins clock a dual-edge asynchronous FIFO: two banks (merged rising/falling) of flop-cell slots loaded through enable muxes (clock never gated), Johnson write pointers; each record holds ring nodes, counters A/B, the pin snapshot and a delayed snapshot. A synchronous drain reads banks in turn, attributes pins by snapshot diff, decodes ring time (`Ring.S.decode`) and flags everything it cannot vouch for: lost edges (per-pin Gray edge counters), level mismatches, stale and unbalanced attributions (edges that cancelled in the XOR), overwrites, runts, invalid pointers (auto-reset), enable changes. `Model` + `Reference`/`Model_driver` for Cyclesim | done. Event_sim with Liberty delays at typ/slow/fast, 105k random edges on 4 pins (10% within 2 ns of the previous edge): zero unflagged errors; calibrated single-edge error 0.8 bin rms (24/37/59 ps fast/typ/slow), worst <= 2.8 bins except rare multi-pin-burst outliers (worst 0.79 ns, followed by a pin flag); snapshot race margin >= 190 ps; two-pin collisions merge (attributed to both) below 250/350/600 ps, cancel (reported untimed, flagged) below ~125-300 ps. Area (yosys, 4 pins, 2 slots/bank, ring excluded): 48.6k µm², of which 14.6k hand-placed front |
| `Strobe_sampler` | Vernier-delayed strobe samples every pin: equivalent-time sampling for repetitive signals | planned |
| Calibration | Code-density histogram of the `clk`-sampled ring; ring periods per cycle | next |
| Behavioural models | Cyclesim models so the rest of a design simulates normally | `Tdc.Model`, `Ring.Model` (driven by `Ring.Ideal`), `Pin_capture.Model` (driven by `Pin_capture.Reference` / `Model_driver`) |
| `Event_sim` | Event-driven gate-level simulation of Hardcaml circuits of Liberty cells: fs time, 0/1/X, NLDM delays with slew propagation and loads, inertial/transport, flops/latches from Liberty with setup/hold-aperture metastability (exponential resolution), corner scale and seeded per-instance variation, Hardcaml registers on any clock net, loops (ring oscillators), VCD | done (minimal); SDF, adversarial delays, hazard/protocol monitors, Cyclesim co-simulation: planned |
| `Sdc` | Timing constraints and LibreLane config (dont-touch, loop breaks, relative timing) | planned |
| Post-route check | Re-extract islands from the routed netlist; check cells survived unchanged | planned |
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

`dune exec test/gate/capture_gate.exe -- EDGES_PER_CORNER [SEED [NEAR [MIN_SAME_PIN_NS [SLOTS]]]]`
is the long Pin_capture gate run (random edges on 4 pins at three corners,
checked against the true edges); `test/yosys/yosys_capture.sh [SLOTS]`
synthesises Pin_capture and reports its area and that every hand-placed
cell survived.

`test/yosys/yosys_tdc.sh` is a synthesis smoke check outside `dune runtest`
(it needs yosys and the full SG13CMOS5L liberty): it synthesises a TDC with
yosys + abc and checks that every delay cell and sampling flop survives.

Test data in `test/data/` is an excerpt of the IHP Open PDK Liberty file
(Apache-2.0).

## License

MIT, like Hardcaml.
