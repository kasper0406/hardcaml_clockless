#!/bin/sh
# Physical-flow check for a stub island (17-stage ring with flop-cell
# counters and sampler, 4-pin Pin_capture, 16-stage TDC; test/yosys/gen_island.ml).
# Not part of `dune runtest`; run by hand:
#
#   test/yosys/yosys_island.sh
#
# Synthesises it with the passes LibreLane's Yosys.Synthesis step runs by
# default (flatten, opt_clean -purge, dfflibmap, abc, hilomap, splitnets,
# insbuf), then checks the netlist with Sdc.check: every protected instance
# survives with its cell type, protected nets are unbuffered, carry the names
# the dont-touch regex matches and have no extra loads, inputs keep their
# sources. Prints the generated regex size and SDC fragments. LIB and YOSYS
# as in yosys_tdc.sh.
set -eu
BITBANGER=/home/user/janestreet-bitbanger
YOSYS=${YOSYS:-$BITBANGER/.venv/bin/yowasp-yosys}
LIB=${LIB:-$BITBANGER/synth/.pdk/sg13cmos5l_stdcell_typ_1p20V_25C.lib}
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
dune build ./test/yosys/gen_island.exe
GEN=$ROOT/_build/default/test/yosys/gen_island.exe
cp "$LIB" "$WORK/cells.lib"
"$GEN" "$WORK/cells.lib" verilog > "$WORK/island.v"

cd "$WORK"
cat > synth.ys <<YS
read_liberty -lib cells.lib
read_verilog island.v
hierarchy -check -top island_top
synth -flatten -top island_top
opt_clean -purge
dfflibmap -liberty cells.lib
abc -liberty cells.lib
hilomap -hicell sg13cmos5l_tiehi L_HI -locell sg13cmos5l_tielo L_LO
splitnets
opt_clean -purge
insbuf -buf sg13cmos5l_buf_1 A X
write_verilog -noattr -noexpr -nohex -nodec -defparam island_synth.v
YS
"$YOSYS" -q -l yosys.log synth.ys || { tail -20 yosys.log; exit 1; }
"$GEN" cells.lib sdc | awk '/^RSZ_DONT_TOUCH_RX/ { print "RSZ_DONT_TOUCH_RX: " length($2) " characters"; next } { print }'
if "$GEN" cells.lib check island_synth.v; then
  echo OK
else
  echo FAILED
  exit 1
fi
