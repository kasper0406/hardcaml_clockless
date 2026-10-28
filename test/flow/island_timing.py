#!/usr/bin/env python3
"""Island timing from SDF: the stub island's (gen_stub.ml) ring, Pin_capture
races and Vernier lines, from an SDF written by OpenSTA (sta_island.tcl):
pre-layout (Liberty "1k" wire load) or routed (SPEF), one corner per file.

    island_timing.py SDF LOOP_SDF [--json OUT] [--spef SPEF]
    island_timing.py --summary DIR     (DIR/{pre,post}-{typ,fast,slow}.json)

LOOP_SDF: the same corner with the ring broken elsewhere (the main SDF has
the ring NAND's feedback arc disabled, Sdc's loop break, which OpenSTA writes
as a zero delay).

A small path calculator over the SDF's interconnect and cell arcs, with
unateness by cell type: earliest arrivals over the SDF's early (min)
delays, latest over its late (max) delays; nominal figures (period, steps,
spans) use the latest. It reports
  - ring: the period (both traversals of the loop; early and late), the
    stage intervals at the ring cells' outputs (the sampler's bins add
    the routing to its flops, their skew and apertures);
  - capture: for every captured pin and bank, the snapshot race margin
    (earliest bank-clock arrival at the slot flops minus the latest arrival
    of the pin's snapshot at their D, minus setup) and the delayed-snapshot
    margin (earliest delayed D minus the latest clock, minus hold);
  - vernier: per line, coarse and fine chain steps (tap to tap), the span
    of each chain, and the launch -> output mux delay range over the taps;
  - with --spef: the extracted wire capacitance of the ring nodes, the
    vernier chain nets and the captured pin nets against the Liberty "1k"
    wire load model's estimate for their fanout (Event_sim's pre-layout
    baseline, D-9).
"""
import collections
import json
import re
import statistics
import sys

POS = "pos"
NEG = "neg"
NON = "non"


def unateness(celltype, inp, out):
    c = celltype.replace("sg13cmos5l_", "")
    if re.match(r"(inv|nand\d|nor\d)_", c):
        return NEG
    if re.match(r"(buf|dlygate\w*)_", c):
        return POS
    if c.startswith("xor2_") or c.startswith("xnor2_"):
        return NON
    if c.startswith("mux2_"):
        return POS if inp in ("A0", "A1") else None  # selects are static
    return None  # anything else: not followed


def unescape(s):
    return s.replace("\\", "")


def val(t):
    # "(0.1::0.2)" -> (0.1, 0.2), early and late ; "()" -> None
    t = t.strip("()")
    if not t:
        return None
    parts = [float(p) for p in t.split(":") if p]
    return (min(parts), max(parts))


def late(d):
    return d[1]


def widen(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return (min(a[0], b[0]), max(a[1], b[1]))


class Sdf:
    def __init__(self, path):
        text = open(path).read()
        self.celltype = {}
        self.arcs = collections.defaultdict(dict)  # inst -> (in, out) -> (rise, fall)
        self.checks = collections.defaultdict(dict)  # inst -> (kind, edge_d, pin) -> value
        self.fanout = collections.defaultdict(list)  # pin -> [(load pin, (rise, fall))]
        self.driver = {}
        triple = r"(\([^()]*\))"
        for cell in re.split(r"\n \(CELL\n", text)[1:]:
            ct = re.search(r'\(CELLTYPE "([^"]+)"\)', cell).group(1)
            inst = re.search(r"\(INSTANCE ?([^)]*)\)", cell).group(1).strip()
            inst = unescape(inst)
            if inst:
                self.celltype[inst] = ct
            for m in re.finditer(r"\(INTERCONNECT (\S+) (\S+) " + triple + r" ?" + triple + r"?\)", cell):
                a, b = m.group(1), m.group(2)
                r = val(m.group(3))
                f = val(m.group(4)) if m.group(4) else r
                fa = self.pin_key(a)
                fb = self.pin_key(b)
                self.fanout[fa].append((fb, (r, f)))
                self.driver[fb] = fa
            for m in re.finditer(r"\(IOPATH (\S+) (\S+) " + triple + r" ?" + triple + r"?\)", cell):
                i, o = m.group(1), m.group(2)
                r = val(m.group(3))
                f = val(m.group(4)) if m.group(4) else r
                old = self.arcs[inst].get((i, o))
                if old is None:
                    self.arcs[inst][(i, o)] = (r, f)
                else:  # conditional variants: the earliest and the latest
                    self.arcs[inst][(i, o)] = (widen(old[0], r), widen(old[1], f))
            for m in re.finditer(r"\((SETUP|HOLD) \((posedge|negedge) (\S+)\) \(posedge (\S+)\) " + triple + r"\)", cell):
                self.checks[inst][(m.group(1), m.group(2), m.group(3))] = late(val(m.group(5)))

    @staticmethod
    def pin_key(ref):
        # instance pins as "inst/pin", ports as their name
        m = re.match(r"^(.*?[^\\])\.([A-Za-z0-9_]+)$", ref)
        if m and not re.match(r"^(ui_in|uio_in|uo_out|uio_out|uio_oe|clk|rst_n|ena)(\[\d+\])?$", unescape(ref)):
            return unescape(m.group(1)) + "/" + m.group(2)
        return unescape(ref)

    def outputs(self, inst, inp):
        return [(o, d) for (i, o), d in self.arcs.get(inst, {}).items() if i == inp]

    def propagate(self, start, edge, t0=0.0, stop=lambda pin: False):
        """Arrival (earliest, latest) per (pin, edge) from [start] (an output
        pin or port) switching with [edge] ('r' or 'f') at t0: the earliest
        over early (SDF min) delays, the latest over late (max) delays."""
        arr = {}
        work = [(start, edge, (t0, t0))]

        def note(pin, e, t):
            lo, hi = arr.get((pin, e), (float("inf"), float("-inf")))
            changed = t[0] < lo or t[1] > hi
            arr[(pin, e)] = (min(lo, t[0]), max(hi, t[1]))
            return changed

        def add(t, d):
            return (t[0] + d[0], t[1] + d[1])

        note(start, edge, (t0, t0))
        while work:
            pin, e, t = work.pop()
            for load, (r, f) in self.fanout.get(pin, []):
                tl = add(t, r if e == "r" else f)
                if not note(load, e, tl) or stop(load) or "/" not in load:
                    continue
                inst, ipin = load.split("/")
                ct = self.celltype.get(inst, "")
                for opin, (orise, ofall) in self.outputs(inst, ipin):
                    u = unateness(ct, ipin, opin)
                    if u is None:
                        continue
                    outs = {POS: [e], NEG: ["f" if e == "r" else "r"], NON: ["r", "f"]}[u]
                    for oe in outs:
                        d = orise if oe == "r" else ofall
                        if d is None:
                            continue
                        out = inst + "/" + opin
                        if note(out, oe, add(tl, d)):
                            work.append((out, oe, add(tl, d)))
        return arr


def ring(sdf, loop):
    p = "island.ring_"
    nand_y = p + "nand/Y"
    # B -> Y of the NAND from the loop SDF (disabled in the main one)
    by = loop.arcs[p + "nand"][("B", "Y")]
    stop = lambda pin: pin == p + "nand/B"
    res = {}
    period = [0.0, 0.0]  # over early, late delays
    stages = []
    for e in "rf":
        arr = sdf.propagate(nand_y, e, stop=stop)
        # node k: output of inv k (k = 1..16), edge alternates
        t_prev = 0.0
        ek = e
        for k in range(1, 17):
            ek = "f" if ek == "r" else "r"
            t = arr[(p + f"inv{k}/Y", ek)][1]
            stages.append(t - t_prev)
            t_prev = t
        tb = arr[(p + "nand/B", ek)]
        back = by[0] if ek == "f" else by[1]  # B falls -> Y rises
        if back is None or back[1] <= 0:
            raise SystemExit("ring: no NAND feedback delay in LOOP_SDF (pass the SDF with the ring broken elsewhere)")
        stages.append(tb[1] - t_prev + back[1])
        period[0] += tb[0] + back[0]
        period[1] += tb[1] + back[1]
    # the early period is the one to time the ring-clocked counters against
    res["period_ns"] = period[1]
    res["period_early_ns"] = period[0]
    res["stage_ps"] = [round(1000 * s, 1) for s in stages]
    # stage intervals at the ring cells' outputs (the sampler's bins also
    # depend on the routing to its flops, their clock skew and apertures)
    res["stage_min_ps"] = round(1000 * min(stages), 1)
    res["stage_max_ps"] = round(1000 * max(stages), 1)
    return res


def capture(sdf):
    p = "island.pin_capture_"
    res = {}
    for i in range(4):
        # the pin net: the port for pins 2-3, the loopback mux output for 0-1
        dpin = p + f"dpin{i}/A"
        src = sdf.driver[dpin]
        slots = sorted({inst for inst in sdf.celltype if re.match(re.escape(p) + r"[rf]_s\d+_pins%d$" % i, inst)})
        dlys = sorted({inst for inst in sdf.celltype if re.match(re.escape(p) + r"[rf]_s\d+_delayed%d$" % i, inst)})
        stop = lambda pin: pin.endswith("/CLK") or pin.endswith("/D")
        out = {}
        for e in "rf":
            arr = sdf.propagate(src, e, stop=stop)
            for bank in "rf":
                race = []
                delayed = []
                for f in slots + dlys:
                    if f"_{bank}_s" not in f:
                        continue
                    clk = arr.get((f + "/CLK", "r"))
                    d = [arr[(f + "/D", x)] for x in "rf" if (f + "/D", x) in arr]
                    if clk is None or not d:
                        raise SystemExit(f"capture: no path from pin {i} ({e}) to {f} (bank {bank})")
                    if f in slots:
                        setup = max(v for (k, _, pin), v in sdf.checks[f].items() if k == "SETUP")
                        race.append(clk[0] - max(x[1] for x in d) - setup)
                    else:
                        hold = max(v for (k, _, pin), v in sdf.checks[f].items() if k == "HOLD")
                        delayed.append(min(x[0] for x in d) - clk[1] - hold)
                if not race or (dlys and not delayed):
                    raise SystemExit(f"capture: pin {i} ({e}), bank {bank}: no slot flops found")
                if race:
                    out[f"pin_{e}_bank_{bank}"] = {
                        "snapshot_margin_ps": round(1000 * min(race), 1),
                        "delayed_margin_ps": round(1000 * min(delayed), 1) if delayed else None,
                    }
        res[f"pin{i}"] = out
    margins = [v["snapshot_margin_ps"] for o in res.values() for v in o.values()]
    delayed = [v["delayed_margin_ps"] for o in res.values() for v in o.values() if v["delayed_margin_ps"] is not None]
    res["snapshot_margin_min_ps"] = min(margins) if margins else None
    res["delayed_margin_min_ps"] = min(delayed) if delayed else None
    return res


def vernier(sdf):
    res = {}
    for pin in range(2):
        for line in "rf":
            p = f"island.vernier_p{pin}_{line}_"
            launch = p + "launch/Q"
            line_res = {}
            for sec, stage_rx, tap0_from in (("coarse", r"c_stage(\d+)_(\d+)$", "launch/Q"), ("fine", r"f_stage(\d+)_(\d+)$", None)):
                stage_insts = collections.defaultdict(list)
                for inst in sdf.celltype:
                    m = re.match(re.escape(p) + stage_rx, inst)
                    if m:
                        stage_insts[int(m.group(1))].append((int(m.group(2)), inst))
                if not stage_insts:
                    continue
                n = max(stage_insts) + 1
                last = [max(stage_insts[k])[1] + "/X" if sdf.celltype[max(stage_insts[k])[1]].find("inv") < 0
                        else max(stage_insts[k])[1] + "/Y" for k in range(n)]
                first_in = min(stage_insts[0])[1] + "/A"
                start = sdf.driver[first_in]
                per_edge = {}
                for e in "rf":
                    arr = sdf.propagate(start, e, stop=lambda pin: pin.endswith("/S"))
                    taps = [0.0] + [arr[(t, e)][1] for t in last]  # every stage is non-inverting
                    steps = [b - a for a, b in zip(taps, taps[1:])]
                    per_edge[e] = steps
                steps = per_edge["r"] + per_edge["f"]
                line_res[sec] = {
                    "taps": n + 1,
                    "step_mean_ps": round(1000 * statistics.mean(steps), 1),
                    "step_min_ps": round(1000 * min(steps), 1),
                    "step_max_ps": round(1000 * max(steps), 1),
                    "span_rise_ns": round(sum(per_edge["r"]), 3),
                    "span_fall_ns": round(sum(per_edge["f"]), 3),
                }
            # launch -> XOR -> output mux: shortest and longest over the taps
            out = f"island.vernier_p{pin}_out/A0"
            a = []
            for e in "rf":  # a launch flop toggles both ways
                arr = sdf.propagate(launch, e, stop=lambda pin: pin.endswith("/S"))
                a += [arr[(out, x)] for x in "rf" if (out, x) in arr]
            if a:
                line_res["launch_to_out_ns"] = {"min": round(min(x[0] for x in a), 3), "max": round(max(x[1] for x in a), 3)}
            res[f"p{pin}_{line}"] = line_res
    return res


# Liberty sg13cmos5l "1k": fanout_length (1, 9.93) (5, 59.59) (20, 215.6),
# slope 10.9, capacitance 0.0002 pF/um.
WL_1K = [(1, 9.93), (5, 59.59), (20, 215.6)]


def wire_1k_pf(fanout):
    pts = WL_1K
    if fanout <= pts[0][0]:
        length = pts[0][1] * fanout / pts[0][0]
    elif fanout >= pts[-1][0]:
        length = pts[-1][1] + 10.9 * (fanout - pts[-1][0])
    else:
        for (f0, l0), (f1, l1) in zip(pts, pts[1:]):
            if f0 <= fanout <= f1:
                length = l0 + (l1 - l0) * (fanout - f0) / (f1 - f0)
                break
    return length * 0.0002


def spef_caps(path):
    """Total parasitic capacitance (pF) per net name."""
    names = {}
    caps = {}
    unit = 1.0
    section = None
    for line in open(path):
        if line.startswith("*C_UNIT"):
            v, u = line.split()[1:3]
            unit = float(v) * {"PF": 1.0, "FF": 1e-3}[u.upper()]
        elif line.startswith("*NAME_MAP"):
            section = "map"
        elif line.startswith("*D_NET"):
            section = None
            _, n, c = line.split()[:3]
            caps[unescape(names.get(n, n))] = float(c) * unit
        elif section == "map" and line.startswith("*"):
            parts = line.split()
            if len(parts) == 2:
                names[parts[0]] = parts[1]
    return caps


def wire_caps(sdf, spef):
    caps = spef_caps(spef)
    groups = {
        "ring nodes": [("island.ring_nand/Y", "island.ring_nand__Y")]
        + [(f"island.ring_inv{k}/Y", f"island.ring_inv{k}__Y") for k in range(1, 17)],
        "vernier coarse chain": [(i + "/" + ("Y" if "inv" in sdf.celltype[i] else "X"), i + ("__Y" if "inv" in sdf.celltype[i] else "__X"))
                                 for i in sorted(sdf.celltype) if re.match(r"island\.vernier_p\d_[rf]_c_stage\d+_\d+$", i)],
        "vernier fine chain": [(i + "/X", i + "__X") for i in sorted(sdf.celltype) if re.match(r"island\.vernier_p\d_[rf]_f_stage\d+_\d+$", i)],
    }
    res = {}
    for g, nets in groups.items():
        rows = []
        for drv, net in nets:
            if net not in caps:
                continue
            fo = len([l for l, _ in sdf.fanout.get(drv, []) if "/" in l])
            rows.append((caps[net], wire_1k_pf(fo)))
        if rows:
            res[g] = {
                "nets": len(rows),
                "spef_ff_mean": round(1000 * statistics.mean(r[0] for r in rows), 2),
                "spef_ff_max": round(1000 * max(r[0] for r in rows), 2),
                "wl1k_ff_mean": round(1000 * statistics.mean(r[1] for r in rows), 2),
                "ratio_mean": round(statistics.mean(r[0] / r[1] for r in rows), 2),
            }
    return res


def summary(d):
    """A pre- against post-layout table of the JSON results in [d]."""
    rows = [
        ("ring period, early-late (ns)", lambda r: f"{r['ring']['period_early_ns']:.2f}-{r['ring']['period_ns']:.2f}"),
        ("ring stage interval (ps)", lambda r: f"{r['ring']['stage_min_ps']:.0f}-{r['ring']['stage_max_ps']:.0f}"),
        ("snapshot race margin (ps)", lambda r: f"{r['capture']['snapshot_margin_min_ps']:.0f}"),
        ("recent-edge delay margin (ps)", lambda r: f"{r['capture']['delayed_margin_min_ps']:.0f}"),
        ("vernier coarse step (ps)", lambda r: "{:.0f}-{:.0f}".format(
            min(v["coarse"]["step_min_ps"] for v in r["vernier"].values()),
            max(v["coarse"]["step_max_ps"] for v in r["vernier"].values()))),
        ("vernier coarse span (ns)", lambda r: "{:.2f}".format(
            min(min(v["coarse"]["span_rise_ns"], v["coarse"]["span_fall_ns"]) for v in r["vernier"].values()))),
        ("vernier fine step (ps)", lambda r: "{:.0f}-{:.0f}".format(
            min(v["fine"]["step_min_ps"] for v in r["vernier"].values()),
            max(v["fine"]["step_max_ps"] for v in r["vernier"].values()))),
        ("vernier fine span (ns)", lambda r: "{:.2f}".format(
            min(min(v["fine"]["span_rise_ns"], v["fine"]["span_fall_ns"]) for v in r["vernier"].values()))),
        ("vernier launch->out max (ns)", lambda r: "{:.2f}".format(
            max(v["launch_to_out_ns"]["max"] for v in r["vernier"].values()))),
    ]
    res = {}
    for stage in ("pre", "post"):
        for c in ("typ", "fast", "slow"):
            try:
                res[(stage, c)] = json.load(open(f"{d}/{stage}-{c}.json"))
            except FileNotFoundError:
                pass
    cols = [(s, c) for c in ("fast", "typ", "slow") for s in ("pre", "post") if (s, c) in res]
    print("| | " + " | ".join(f"{c} {s}" for s, c in cols) + " |")
    print("|---" * (len(cols) + 1) + "|")
    for name, f in rows:
        print(f"| {name} | " + " | ".join(f(res[k]) for k in cols) + " |")
    post = next((res[k] for k in cols if k[0] == "post" and "wire_caps" in res[k]), None)
    if post:
        print()
        print("| net group (extracted vs Liberty 1k wire cap) | extracted mean (max), fF | 1k mean, fF | ratio |")
        print("|---|---|---|---|")
        for g, v in post["wire_caps"].items():
            print(f"| {g} ({v['nets']}) | {v['spef_ff_mean']} ({v['spef_ff_max']}) | {v['wl1k_ff_mean']} | {v['ratio_mean']} |")


def main():
    args = sys.argv[1:]
    if args[:1] == ["--summary"]:
        summary(args[1])
        return
    out = None
    spef = None
    if "--json" in args:
        k = args.index("--json")
        out = args[k + 1]
        del args[k:k + 2]
    if "--spef" in args:
        k = args.index("--spef")
        spef = args[k + 1]
        del args[k:k + 2]
    sdf = Sdf(args[0])
    if len(args) != 2:
        raise SystemExit(__doc__)
    loop = Sdf(args[1])
    res = {"ring": ring(sdf, loop), "capture": capture(sdf), "vernier": vernier(sdf)}
    if spef:
        res["wire_caps"] = wire_caps(sdf, spef)
    if out:
        json.dump(res, open(out, "w"), indent=1)
    r = res["ring"]
    print(f"ring: period {r['period_early_ns']:.3f}-{r['period_ns']:.3f} ns, stage interval {r['stage_min_ps']}-{r['stage_max_ps']} ps")
    c = res["capture"]
    print(f"capture: snapshot race margin >= {c['snapshot_margin_min_ps']} ps, delayed-snapshot margin >= {c['delayed_margin_min_ps']} ps")
    for i in range(4):
        print(f"  pin{i}: " + ", ".join(f"{k} {v['snapshot_margin_ps']}/{v['delayed_margin_ps']}" for k, v in c[f"pin{i}"].items()))
    for k, v in res["vernier"].items():
        cs, fs = v.get("coarse", {}), v.get("fine", {})
        print(f"vernier {k}: coarse {cs.get('taps')} taps step {cs.get('step_mean_ps')} ps ({cs.get('step_min_ps')}-{cs.get('step_max_ps')}), "
              f"span {cs.get('span_rise_ns')}/{cs.get('span_fall_ns')} ns; fine step {fs.get('step_mean_ps')} ps "
              f"({fs.get('step_min_ps')}-{fs.get('step_max_ps')}), span {fs.get('span_rise_ns')}/{fs.get('span_fall_ns')} ns; "
              f"launch->out {v.get('launch_to_out_ns')}")
    for g, v in res.get("wire_caps", {}).items():
        print(f"wire cap, {g} ({v['nets']} nets): extracted {v['spef_ff_mean']} fF mean ({v['spef_ff_max']} max), "
              f"Liberty 1k {v['wl1k_ff_mean']} fF mean; ratio {v['ratio_mean']}")


if __name__ == "__main__":
    main()
