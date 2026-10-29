#!/usr/bin/env python3
"""Island timing from SDF: the stub island's (gen_stub.ml) ring, Pin_capture
races and Vernier lines, from an SDF written by OpenSTA (sta_island.tcl):
pre-layout (Liberty "1k" wire load) or routed (SPEF), one corner per file.

    island_timing.py SDF LOOP_SDF [--json OUT] [--spef SPEF] [--names RING,SAMPLER,CAPTURE,VERNIER]
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
import math
import re
import statistics
import sys

# Instance name prefixes of the structures (the stub island's by default;
# --names RING,SAMPLER,CAPTURE,VERNIER for another design, e.g. the v1 chip:
# capture.capture_ring_,capture.capture_clk_sample_,capture.capture_pc_,protoemu_vernier.vernier_).
RING = "island.ring_"
SAMPLER = "island.ring_sample_"
CAPTURE = "island.pin_capture_"
VERNIER = "island.vernier_"


def scope(prefix):
    """The part of a prefix naming the island's hierarchy ("island." of
    "island.ring_"), or the whole prefix if it has none."""
    return prefix[: prefix.rfind(".") + 1] if "." in prefix else prefix


def hand(inst):
    """A hand-placed island cell (not synthesised logic)."""
    return any(inst.startswith(scope(x)) for x in (RING, SAMPLER, CAPTURE, VERNIER))


POS = "pos"
NEG = "neg"
NON = "non"


def unateness(celltype, inp, out, selects=False):
    c = celltype.replace("sg13cmos5l_", "")
    if c.startswith("dfrbpq_"):
        # Q after the rising clock edge (only), either way
        return NON if (selects and inp == "CLK") else None
    if re.match(r"(inv|nand\d|nor\d)_", c):
        return NEG
    if re.match(r"(buf|dlygate\w*)_", c):
        return POS
    if c.startswith("xor2_") or c.startswith("xnor2_"):
        return NON
    if c.startswith("mux2_"):
        if inp in ("A0", "A1"):
            return POS
        return NON if selects else None  # selects are static, unless asked
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

    def propagate(self, start, edge, t0=0.0, stop=lambda pin: False, selects=False, within=None):
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
                if within is not None and not within(inst):
                    continue
                ct = self.celltype.get(inst, "")
                for opin, (orise, ofall) in self.outputs(inst, ipin):
                    if ipin == "CLK" and e != "r":
                        continue
                    u = unateness(ct, ipin, opin, selects)
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
    p = RING
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
    p = CAPTURE
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


def node_waveform(sdf, loop, n=17):
    """Per ring transition q in [0, 2n) (q = 0 a rise of node 0, node q mod n
    changes): its edge and time at the node, over late delays; the period."""
    p = RING
    by = loop.arcs[p + "nand"][("B", "Y")]
    stop = lambda pin: pin == p + "nand/B"
    times = []
    t0 = 0.0
    for e in "rf":
        arr = sdf.propagate(p + "nand/Y", e, t0=t0, stop=stop)
        times.append((p + "nand/Y", e, t0))
        ek = e
        for k in range(1, n):
            ek = "f" if ek == "r" else "r"
            times.append((p + f"inv{k}/Y", ek, arr[(p + f"inv{k}/Y", ek)][1]))
        tb = arr[(p + "nand/B", ek)][1]
        back = by[0] if ek == "f" else by[1]  # B falls -> Y rises
        t0 = tb + back[1]
    return times, t0


def counter_margins(sdf, loop, flops, frame, n=17, settle=9, earliest=4, periods=6):
    """Ring_margins.counter (test/ring_margins.ml) on static delays: the
    counter-selection margins, in stages, of a sampler whose flops are
    [flops] (role -> flop: ("node", k) or ("a"|"b", bit)), with each flop's
    clock arrival [frame(flop)] subtracted. Every counter bit is taken to
    change at every edge (worst case)."""
    p = RING
    times, period = node_waveform(sdf, loop, n)

    def check(f, kind, edge):
        return max(v for (k, ed, pin), v in sdf.checks[f].items() if k == kind and ed == ("posedge" if edge == "r" else "negedge"))

    # stop at flops, and at the next ring stage (the ring is a loop)
    at_d = lambda pin: (pin.endswith("/D") or pin.endswith("/CLK")
                        or re.match(re.escape(p) + r"(inv\d+/A|nand/B)$", pin) is not None)
    # node transitions as the flops see them (one period, then shifted)
    one = []
    for q, (src, e, t) in enumerate(times):
        f = flops[("node", q % n)]
        arr = sdf.propagate(src, e, t0=t, stop=at_d, within=hand)
        d = {x: arr[(f + "/D", x)][1] for x in "rf" if (f + "/D", x) in arr}
        if not d:
            raise SystemExit(f"margins: no path from {src} to {f}/D")
        x = max(d, key=d.get)
        one.append(dict(time=d[x] - frame(f), setup=check(f, "SETUP", x), hold=check(f, "HOLD", x)))
    tr = [dict(c, time=c["time"] + j * period) for j in range(periods) for c in one]
    # counter changes: each counter flop's clock from node 0, clk->Q, to the flops
    changes = {"a": [], "b": []}
    for tag, first in (("a", 0), ("b", n)):
        src, e, t = times[first]
        clk_at = sdf.propagate(src, e, t0=t, stop=at_d, within=hand)
        once = []
        hit = set()
        for cnt in [i for i in sdf.celltype if i.startswith(p + f"cnt_{tag}_")]:
            c = clk_at.get((cnt + "/CLK", "r"))
            if c is None:
                raise SystemExit(f"margins: no clock path from node 0 to {cnt}")
            for qe in "rf":
                cq = sdf.arcs[cnt][("CLK", "Q")][0 if qe == "r" else 1]
                arr = sdf.propagate(cnt + "/Q", qe, t0=c[1] + cq[1], stop=at_d, within=hand)
                arr_lo = c[0] + cq[0]
                for (role, bit), f in flops.items():
                    if role != tag:
                        continue
                    for x in "rf":
                        if (f + "/D", x) in arr:
                            lo, hi = arr[(f + "/D", x)]
                            hit.add(f)
                            once.append(dict(time=hi - frame(f), lo=lo - (c[1] + cq[1]) + arr_lo - frame(f),
                                             setup=check(f, "SETUP", x), hold=check(f, "HOLD", x)))
        missing = [f for (role, _), f in flops.items() if role == tag and f not in hit]
        if missing:
            raise SystemExit(f"margins: no path from counter {tag} to {missing}")
        changes[tag] = [dict(c, time=c["time"] + j * period, lo=c["lo"] + j * period) for j in range(periods) for c in once]
    seen = [x["time"] + x["setup"] for x in tr]
    bins = [seen[q + 1] - seen[q] for q in range(2 * n * (periods - 1))]
    inner = [b for q, b in enumerate(bins) if q % n != n - 1]
    stage = sum(inner) / len(inner)

    def med3(f, i):
        return sorted([f(i - 1), f(i), f(i + 1)])[1]

    can_new = lambda i: tr[i]["time"] - tr[i]["hold"]
    can_old = lambda i: tr[i]["time"] + tr[i]["setup"]
    r_lo = lambda q: max(med3(can_new, q), med3(can_new, q - 1))
    r_hi = lambda q: min(med3(can_old, q + 1), med3(can_old, q + 2))
    s = (n + settle + earliest + 1) // 2
    res = {}
    for tag, first in (("a", 0), ("b", n)):
        edges = [first + 2 * n * j for j in range(periods)]
        unstable = {}
        for c in changes[tag]:
            j = max(i for i, q in enumerate(edges) if tr[q]["time"] <= c["lo"] + 1e-9) if any(tr[q]["time"] <= c["lo"] + 1e-9 for q in edges) else None
            if j is None:
                continue
            lo, hi = c["lo"] - c["hold"], c["time"] + c["setup"]
            a, b = unstable.get(j, (lo, hi))
            unstable[j] = (min(a, lo), max(b, hi))
        before = after = float("inf")
        unsafe = []
        for j in range(1, periods - 2):
            if j not in unstable or j + 1 not in unstable:
                continue
            lo, hi = unstable[j]
            lo_next = unstable[j + 1][0]
            for d in range(-n, n):
                q = edges[j] + d
                if r_lo(q) < hi and r_hi(q) > lo:
                    unsafe.append(d)
            for q in range(edges[j] + s, edges[j] + s + n):
                before = min(before, r_lo(q) - hi)
                after = min(after, lo_next - r_hi(q))
        res[tag] = {"safe_from": max(unsafe) + 1 if unsafe else None,
                    "before_stages": round(before / stage, 1), "after_stages": round(after / stage, 1),
                    "worst_exact": min(before, after) / stage}
    res["worst_stages"] = min(min(v["before_stages"], v["after_stages"]) for k, v in res.items() if k in "ab")
    res["worst_exact"] = min(v["worst_exact"] for k, v in res.items() if k in "ab")
    if not all(math.isfinite(v["before_stages"]) and math.isfinite(v["after_stages"]) for k, v in res.items() if k in "ab"):
        raise SystemExit("margins: no complete period (missing counter paths?)")
    res["stage_ps"] = round(1000 * stage, 1)
    return res


def clock_arrival(sdf, root, edge="r"):
    """Arrival of [root]'s edge at every flop clock pin it reaches."""
    # the clock tree: CTS buffers and hand-placed cells, not synthesised logic
    arr = sdf.propagate(root, edge, stop=lambda pin: pin.endswith("/CLK"),
                        within=lambda inst: not re.match(r"_\d+_$", inst))
    return {pin[: -len("/CLK")]: v[1] for (pin, e), v in arr.items() if pin.endswith("/CLK") and e == "r"}


def ring_margins(sdf, loop):
    res = {}
    # the system-clock sampler: first-stage flop cells on clk
    clk = clock_arrival(sdf, "clk")
    flops = {("node", k): f"{SAMPLER}node{k}" for k in range(17)}
    for tag in "ab":
        for i in range(10):
            f = f"{SAMPLER}count_{tag}_{i}"
            if f in sdf.celltype:
                flops[(tag, i)] = f
    res["clk_sampler"] = counter_margins(sdf, loop, flops, lambda f: clk[f])
    # Pin_capture's slots, per bank and slot, against their own bank clock
    pc = CAPTURE
    for bank, root in (("r", pc + "clkr/X"), ("f", pc + "clkf/Y")):
        ck = clock_arrival(sdf, root)
        for slot in range(4):
            base = f"{pc}{bank}_s{slot}_"
            if base + "nodes0" not in sdf.celltype:
                continue
            flops = {("node", k): f"{base}nodes{k}" for k in range(17)}
            for tag, nm in (("a", "count_a"), ("b", "count_b")):
                for i in range(10):
                    f = f"{base}{nm}{i}"
                    if f in sdf.celltype:
                        flops[(tag, i)] = f
            res[f"capture_{bank}{slot}"] = counter_margins(sdf, loop, flops, lambda f, ck=ck: ck[f])
    res["worst_stages"] = min(v["worst_stages"] for v in res.values())
    res["worst_exact"] = min(v["worst_exact"] for k, v in res.items() if isinstance(v, dict))
    return res


def tag_lag(sdf):
    """Pin_capture: per bank and slot, the tag-lag margin as
    Capture_timing.tag_margins defines it, on static delays: after a bank
    clock edge (the pointer moves, the slot select follows, the tag's
    select lags), the earliest tag D (through its mux's select, S -> X ->
    D) less its hold, minus the latest other D plus its setup, each in its
    own flop's clock frame (clock arrival after the trunk). Over early and
    over late delays (one event: not mixed)."""
    pc = CAPTURE
    res = {}
    slot_rx = re.compile(r"_[rf]_s\d+_")
    exact = []
    for bank, root in (("r", pc + "clkr/X"), ("f", pc + "clkf/Y")):
        arr = sdf.propagate(root, "r", selects=True,
                            stop=lambda pin: (pin.endswith("/S") or pin.endswith("/D")
                                              or (pin.endswith("/CLK") and slot_rx.search(pin) is not None)),
                            within=lambda inst: inst.startswith(pc))

        def at(pin, k):
            v = [arr[(pin, x)][k] for x in "rf" if (pin, x) in arr]
            if not v:
                raise SystemExit(f"tag lag: no path from {root} to {pin}")
            return min(v) if k == 0 else max(v)

        def fresh(f, k):
            m = f + "_mux"
            sx = sdf.arcs[m][("S", "X")]
            wire = next(d for load, d in sdf.fanout[m + "/X"] if load == f + "/D")
            pick = min if k == 0 else max
            return at(m + "/S", k) + pick(x[k] for x in sx if x) + wire[0][k]

        def check(f, kind):
            return max(v for key, v in sdf.checks[f].items() if key[0] == kind)

        for slot in range(4):
            base = f"{pc}{bank}_s{slot}_"
            flops = [i[: -len("_mux")] for i in sdf.celltype if i.startswith(base) and i.endswith("_mux")]
            if not flops:
                continue
            tags = [f for f in flops if re.match(re.escape(base) + r"tag\d+$", f)]
            data = [f for f in flops if f not in tags]
            if not tags or not data:
                raise SystemExit(f"tag lag: slot {base} without tags or data")
            margin = min(
                min(fresh(f, k) - at(f + "/CLK", k) - check(f, "HOLD") for f in tags)
                - max(fresh(f, k) - at(f + "/CLK", k) + check(f, "SETUP") for f in data)
                for k in (0, 1))
            res[f"{bank}{slot}"] = round(1000 * margin, 1)
            exact.append(1000 * margin)
    if not res:
        raise SystemExit("tag lag: no capture slots")
    res["worst_ps"] = min(res.values())
    res["worst_exact_ps"] = min(exact)  # unrounded, for thresholds
    return res


def vernier(sdf):
    res = {}
    for pin in range(2):
        for line in "rf":
            p = f"{VERNIER}p{pin}_{line}_"
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
            out = f"{VERNIER}p{pin}_out/A0"
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
        "ring nodes": [(RING + "nand/Y", RING + "nand__Y")]
        + [(f"{RING}inv{k}/Y", f"{RING}inv{k}__Y") for k in range(1, 17)],
        "vernier coarse chain": [(i + "/" + ("Y" if "inv" in sdf.celltype[i] else "X"), i + ("__Y" if "inv" in sdf.celltype[i] else "__X"))
                                 for i in sorted(sdf.celltype) if re.match(re.escape(VERNIER) + r"p\d_[rf]_c_stage\d+_\d+$", i)],
        "vernier fine chain": [(i + "/X", i + "__X") for i in sorted(sdf.celltype) if re.match(re.escape(VERNIER) + r"p\d_[rf]_f_stage\d+_\d+$", i)],
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
        ("counter selection margin (stages)", lambda r: f"{r['ring_margins']['worst_stages']:.1f}"),
        ("tag lag over setup + hold (ps)", lambda r: f"{r['tag_lag']['worst_ps']:.0f}"),
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
    if "--names" in args:
        k = args.index("--names")
        global RING, SAMPLER, CAPTURE, VERNIER
        RING, SAMPLER, CAPTURE, VERNIER = args[k + 1].split(",")
        del args[k:k + 2]
    if "--spef" in args:
        k = args.index("--spef")
        spef = args[k + 1]
        del args[k:k + 2]
    sdf = Sdf(args[0])
    if len(args) != 2:
        raise SystemExit(__doc__)
    loop = Sdf(args[1])
    res = {"ring": ring(sdf, loop), "capture": capture(sdf), "vernier": vernier(sdf),
           "ring_margins": ring_margins(sdf, loop), "tag_lag": tag_lag(sdf)}
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
    rm = res["ring_margins"]
    print(f"counter selection (L/E) margins: worst {rm['worst_stages']} stages; "
          + ", ".join(f"{k} {v['worst_stages']}" for k, v in rm.items() if isinstance(v, dict)))
    print(f"tag lag over setup + hold: worst {res['tag_lag']['worst_ps']} ps")
    for g, v in res.get("wire_caps", {}).items():
        print(f"wire cap, {g} ({v['nets']} nets): extracted {v['spef_ff_mean']} fF mean ({v['spef_ff_max']} max), "
              f"Liberty 1k {v['wl1k_ff_mean']} fF mean; ratio {v['ratio_mean']}")


if __name__ == "__main__":
    main()
