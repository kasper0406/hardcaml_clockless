#!/usr/bin/env python3
"""Cut a test excerpt out of an IHP SG13CMOS5L stdcell liberty file.

Usage: make_excerpt.py FULL.lib OUT.lib

Keeps the library header, wire loads and the cells in CELLS; drops
operating_conditions, table templates (the tables carry their own indices),
power templates and all power groups. This is how the *_excerpt*.lib files
next to this script were made.
"""
import re
import sys

CELLS = """inv_1 buf_1 nand2_1 nor2_1 o21ai_1 a21oi_1 xor2_1 mux2_1 dlygate4sd1_1
dlygate4sd3_1 dlhq_1 dfrbpq_1 ebufn_2 tiehi fill_1""".split()
CELLS = ["sg13cmos5l_" + c for c in CELLS]
DROP_TOP = {"operating_conditions", "lu_table_template", "power_lut_template"}
DROP_ANY = {"leakage_power", "internal_power"}
GROUP = re.compile(r"^\s*(\w+)\s*\(([^)]*)\)\s*\{\s*$")


def main(src, dst):
    lines = open(src).read().split("\n")
    out = []
    depth = 0
    skip_until = None  # depth at which a dropped group closes
    for line in lines:
        m = GROUP.match(line)
        opens = line.count("{")
        closes = line.count("}")
        if skip_until is None and m:
            kind, arg = m.group(1), m.group(2).strip().strip('"')
            drop = (kind in DROP_ANY) or (depth == 1 and kind in DROP_TOP) or (
                depth == 1 and kind == "cell" and arg not in CELLS)
            if drop:
                skip_until = depth
        if skip_until is None:
            out.append(line)
        depth += opens - closes
        if skip_until is not None and depth == skip_until and closes:
            skip_until = None
    header = ("/* Excerpt of %s from the IHP Open PDK\n"
              "   (https://github.com/IHP-GmbH/IHP-Open-PDK), used as test data.\n"
              "   Power tables removed. Original license follows. */\n\n") % src.split("/")[-1]
    text = "\n".join(out)
    text = re.sub(r"\n\s*\n(\s*\n)+", "\n\n", text)
    open(dst, "w").write(header + text)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
