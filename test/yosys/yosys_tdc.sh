#!/bin/sh
# Synthesis smoke check for Tdc. Not part of `dune runtest` (yosys is not a
# build dependency); run by hand:
#
#   test/yosys/yosys_tdc.sh [LENGTH [CELL]]    (default: 128 sg13cmos5l_buf_1)
#
# Generates a Cells-mode TDC (module protoemu_tdc under a wrapper), runs
# yosys synth -flatten + dfflibmap + abc against the full IHP SG13CMOS5L
# liberty and checks that
#   - all LENGTH delay cells survive, as CELL, with their tdc_stage<i> names;
#   - all LENGTH+1 first-stage sampling nets tdc_sample<i> survive, each
#     driven by the Q pin of a flop.
# It prints the cell statistics (area from the liberty) and exits non-zero on
# failure.
#
# LIB: the full typ-corner liberty. ABC does not accept the excerpt in
# test/data. Defaults to the copy janestreet-bitbanger's synth/synth.sh
# downloads.
# YOSYS: defaults to the WASM build in the janestreet-bitbanger venv, which
# only sees files under its working directory, so inputs are copied into a
# temporary directory first.
set -eu
LENGTH=${1:-128}
CELL=${2:-sg13cmos5l_buf_1}
BITBANGER=/home/user/janestreet-bitbanger
YOSYS=${YOSYS:-$BITBANGER/.venv/bin/yowasp-yosys}
LIB=${LIB:-$BITBANGER/synth/.pdk/sg13cmos5l_stdcell_typ_1p20V_25C.lib}
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
dune build ./test/yosys/gen_tdc.exe
cp "$LIB" "$WORK/cells.lib"
./_build/default/test/yosys/gen_tdc.exe "$WORK/cells.lib" "$LENGTH" "$CELL" > "$WORK/tdc.v"

cd "$WORK"
cat > synth.ys <<YS
read_liberty -lib cells.lib
read_verilog tdc.v
synth -top tdc_top -flatten
dfflibmap -liberty cells.lib
abc -liberty cells.lib
opt_clean
tee -o stat.txt stat -liberty cells.lib
tee -q -o stages.txt select -list t:$CELL n:*tdc_stage* %i
tee -q -o samples.txt select -list t:*df* %x:+[Q] w:*tdc_sample* %i
YS
"$YOSYS" -q -l yosys.log synth.ys || { tail -20 yosys.log; exit 1; }
sed -n '/cells$/,$p' stat.txt

stages=$(grep -c tdc_stage stages.txt || true)
samples=$(grep -c tdc_sample samples.txt || true)
echo "delay cells $CELL named tdc_stage*: $stages (expected $LENGTH)"
echo "nets tdc_sample* driven by a flop Q: $samples (expected $((LENGTH + 1)))"
if [ "$stages" -eq "$LENGTH" ] && [ "$samples" -eq "$((LENGTH + 1))" ]; then
  echo OK
else
  echo FAILED
  exit 1
fi
