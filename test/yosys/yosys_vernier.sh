#!/bin/sh
# Area and survival check for Vernier (2 pins, the chip configuration). Not
# part of `dune runtest`; run by hand:
#
#   test/yosys/yosys_vernier.sh
#
# Synthesises protoemu_vernier with yosys + abc against the full IHP
# SG13CMOS5L liberty, prints the cell statistics, the area of the
# hand-placed cells (keep attribute) and of one fraction -> tap mapper, and
# checks every hand-placed cell survived. The calibration constants and
# mode bits are inputs (the chip's host-config block holds them): their
# storage is not included.
set -eu
BITBANGER=/home/user/janestreet-bitbanger
YOSYS=${YOSYS:-$BITBANGER/.venv/bin/yowasp-yosys}
LIB=${LIB:-$BITBANGER/synth/.pdk/sg13cmos5l_stdcell_typ_1p20V_25C.lib}
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
dune build ./test/yosys/gen_vernier.exe
cp "$LIB" "$WORK/cells.lib"
./_build/default/test/yosys/gen_vernier.exe "$WORK/cells.lib" vernier > "$WORK/vernier.v"
./_build/default/test/yosys/gen_vernier.exe "$WORK/cells.lib" map > "$WORK/map.v"
placed=$(grep -c 'dont_touch' "$WORK/vernier.v" || true)

cd "$WORK"
for part in vernier map; do
  cat > "synth_$part.ys" <<YS
read_liberty -lib cells.lib
read_verilog $part.v
synth -top vernier_top -flatten
dfflibmap -liberty cells.lib
abc -liberty cells.lib
opt_clean
tee -o stat_$part.txt stat -liberty cells.lib
tee -o placed_$part.txt stat -liberty cells.lib a:keep
tee -q -o placed_list_$part.txt select -list a:keep
YS
  "$YOSYS" -q -l "yosys_$part.log" "synth_$part.ys" || { tail -20 "yosys_$part.log"; exit 1; }
done
sed -n '/Chip area/p;/cells$/,/Chip/p' stat_vernier.txt
echo "hand-placed (keep):"
grep 'Chip area' placed_vernier.txt
echo "one mapper (per pin):"
grep 'Chip area' stat_map.txt
survived=$(grep -c . placed_list_vernier.txt || true)
echo "hand-placed cells in Verilog: $placed; kept after synthesis: $survived"
if [ "$placed" -eq "$survived" ]; then echo OK; else echo FAILED; exit 1; fi
