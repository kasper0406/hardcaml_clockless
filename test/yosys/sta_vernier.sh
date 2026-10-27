#!/bin/sh
# Static timing check of Sdc.vernier's constraints on the chip's Vernier
# (2 pins, flattened). Not part of `dune runtest`; run by hand:
#
#   test/yosys/sta_vernier.sh [PERIOD_NS [SETTLE_CYCLES]]     (default 20 4)
#
# Synthesises vernier_top with yosys + abc against the SG13CMOS5L liberty
# (no parasitics: the liberty's "1k" wire load, as in the Event_sim
# numbers), appends the signoff SDC from
# Sdc.vernier to a system clock on [clock], and reports with OpenSTA:
#   - select bound: the worst path to a tap/bypass mux select pin, against
#     one period;
#   - drain bound: the worst path from a pin's launch flops through the XOR
#     to the output mux, against SETTLE_CYCLES periods;
#   - that nothing else is timed from a launch flop (to the pin, the
#     loopback ports, registers), nor from a select pin on, and that the
#     bypass path to the pin still is.
# YOSYS and STA may be commands with arguments (e.g. a container run that
# mounts the work directory at the same path).
set -eu
PERIOD=${1:-20}
SETTLE=${2:-4}
BITBANGER=/home/user/janestreet-bitbanger
YOSYS=${YOSYS:-$BITBANGER/.venv/bin/yowasp-yosys}
STA=${STA:-sta}
LIB=${LIB:-$BITBANGER/synth/.pdk/sg13cmos5l_stdcell_typ_1p20V_25C.lib}
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=${WORK:-$(mktemp -d)}
mkdir -p "$WORK"

cd "$ROOT"
dune build ./test/yosys/gen_vernier.exe
cp "$LIB" "$WORK/cells.lib"
./_build/default/test/yosys/gen_vernier.exe "$WORK/cells.lib" flat > "$WORK/vernier.v"
./_build/default/test/yosys/gen_vernier.exe "$WORK/cells.lib" sdc "$PERIOD" "$SETTLE" > "$WORK/vernier.sdc"

cd "$WORK"
cat > synth.ys <<YS
read_liberty -lib cells.lib
read_verilog vernier.v
synth -top vernier_top -flatten
dfflibmap -liberty cells.lib
abc -liberty cells.lib
opt_clean
write_verilog -noattr netlist.v
YS
$YOSYS -q -l yosys.log synth.ys || { tail -20 yosys.log; exit 1; }

cat > sta.tcl <<TCL
read_liberty cells.lib
read_verilog netlist.v
link_design vernier_top
set_wire_load_model -name 1k
create_clock -name clk -period $PERIOD [get_ports clock]
set_input_delay 0 -clock clk [delete_from_list [all_inputs] [get_ports clock]]
set_output_delay 0 -clock clk [all_outputs]
source vernier.sdc
puts "== select bound (one period), worst per line"
foreach l {p0_r p0_f p1_r p1_f} {
  report_checks -to [get_pins vernier_\${l}_*/S] -format end -fields {}
}
puts "== drain bound ($SETTLE periods), worst per pin"
foreach p {p0 p1} {
  report_checks -from [get_cells vernier_\${p}_*_launch] -to [get_pins vernier_\${p}_out/A0] -format end
}
puts "== a tap select (from its select register)"
report_checks -to [get_pins vernier_p0_r_cm1_0/S] -fields {} -digits 2
puts "== launch flop to the pin, the loopback ports, registers (cut)"
report_checks -from [get_cells vernier_p0_r_launch] -to [get_ports {out_0 loopback_0 line_out_0}] -unconstrained
report_checks -from [get_cells vernier_p0_r_launch] -to [all_registers] -unconstrained
puts "== launch flop: every endpoint"
report_checks -from [get_cells vernier_p0_r_launch] -unconstrained -format end
puts "== from a select pin on (cut)"
report_checks -through [get_pins vernier_p0_r_cm1_0/S] -to [get_ports out_0] -unconstrained
puts "== bypass: level flop to the pin (timed)"
report_checks -from [get_cells vernier_p0_level] -to [get_ports out_0] -format end
puts "== violations"
report_check_types -max_delay -violators
exit
TCL
# Sdc.vernier's internal endpoints draw OpenSTA warning 1551 ("not a valid
# endpoint") on reading; they are honoured (the reports below use them).
$STA -no_init -exit sta.tcl > sta.log 2>&1 || { tail -20 sta.log; exit 1; }
echo "OpenSTA warnings 1551 (internal endpoints): $(grep -c 'Warning 1551' sta.log || true)"
grep -v 'Warning 1551' sta.log
