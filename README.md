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

It is developed for [janestreet-bitbanger](https://github.com/kasper0406/janestreet-bitbanger),
a protocol-emulator ASIC on IHP SG13CMOS5L via Tiny Tapeout. There, a TDC
gives edge timestamps at ~0.1 ns resolution against a 20 ns clock. The design
rationale and the survey of existing async tools (ACT, Workcraft, Balsa/Teak,
click elements) are in that repo's
[docs/CLOCKLESS.md](https://github.com/kasper0406/janestreet-bitbanger/blob/main/docs/CLOCKLESS.md).

## Status

| Module | What | State |
|---|---|---|
| `Liberty` | Liberty (.lib) parser: groups, attributes, tables | done |
| `Bool_expr` | Liberty boolean functions: parse, evaluate, lower to Hardcaml logic | done |
| `Cell_library` | Typed cells: kind (combinational/latch/flop/tristate/constant/physical), pins, functions, area | done |
| `Cell` | Hand-placed cell instantiation with `dont_touch`/`keep`, pin checking, stable instance names | done |
| `Delay_line` | Tapped chain of buffer/delay cells | done |
| `Tdc` | Delay line + first-stage sampling flops (kept, stably named) + synchronous decoder: bubble filter, fine code, overflow and multi-edge flags, coarse counter. `Cells` (Verilog) or behavioural `Model` (Cyclesim) | done |
| `Tdc_calibration` | Code-density calibration against the system clock | next |
| Behavioural models | Cyclesim models of TDC/delay lines so the rest of a design simulates normally | partial: TDC (`Tdc.Model`, uniform stage delays) |
| `Event_sim` | Event-driven gate-level simulation with Liberty/SDF delays, random and adversarial | planned |
| `Sdc` | Timing constraints and LibreLane config (dont-touch, loop breaks, relative timing) | planned |
| Post-route check | Re-extract islands from the routed netlist; check cells survived unchanged | planned |
| `Click`, `Bd_channel` | Bundled-data click handshake components with a synchronous reference | planned |
| Workcraft export | Formal checks: deadlock freedom, hazard freedom, conformation | planned |

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

`test/yosys/yosys_tdc.sh` is a synthesis smoke check outside `dune runtest`
(it needs yosys and the full SG13CMOS5L liberty): it synthesises a TDC with
yosys + abc and checks that every delay cell and sampling flop survives.

Test data in `test/data/` is an excerpt of the IHP Open PDK Liberty file
(Apache-2.0).

## License

MIT, like Hardcaml.
