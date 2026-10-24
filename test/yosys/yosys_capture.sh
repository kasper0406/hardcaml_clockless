#!/bin/sh
# Area and survival check for Pin_capture (4 pins, ring state from ports).
# Not part of `dune runtest`; run by hand:
#
#   test/yosys/yosys_capture.sh [SLOTS]      (default 2)
#
# Synthesises protoemu_pin_capture with yosys + abc against the full IHP
# SG13CMOS5L liberty, prints the cell statistics split into the hand-placed
# front (dont_touch cells, instance names cap..._) and the synthesised drain,
# and checks every hand-placed cell survived.
set -eu
SLOTS=${1:-2}
BITBANGER=/home/user/janestreet-bitbanger
YOSYS=${YOSYS:-$BITBANGER/.venv/bin/yowasp-yosys}
LIB=${LIB:-$BITBANGER/synth/.pdk/sg13cmos5l_stdcell_typ_1p20V_25C.lib}
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
dune build ./test/yosys/gen_capture.exe
cp "$LIB" "$WORK/cells.lib"
./_build/default/test/yosys/gen_capture.exe "$WORK/cells.lib" "$SLOTS" > "$WORK/cap.v"
placed=$(grep -c 'dont_touch' "$WORK/cap.v" || true)

cd "$WORK"
cat > synth.ys <<YS
read_liberty -lib cells.lib
read_verilog cap.v
synth -top capture_top -flatten
dfflibmap -liberty cells.lib
abc -liberty cells.lib
opt_clean
tee -o stat.txt stat -liberty cells.lib
tee -q -o placed.txt select -list n:*pin_capture_* t:sg13cmos5l_* %i
YS
"$YOSYS" -q -l yosys.log synth.ys || { tail -20 yosys.log; exit 1; }
sed -n '/Chip area/p;/cells$/,/Chip/p' stat.txt
survived=$(grep -c . placed.txt || true)
echo "hand-placed cells in Verilog: $placed; named pin_capture_* after synthesis: $survived"
