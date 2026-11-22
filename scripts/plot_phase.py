"""Plot docs/phase_transition.csv (from bench/phase_transition.exe).

usage: python scripts/plot_phase.py [csv] [output_png]
"""

import csv
import sys
from collections import defaultdict
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

src = Path(sys.argv[1] if len(sys.argv) > 1 else "docs/phase_transition.csv")
out = Path(sys.argv[2] if len(sys.argv) > 2 else "docs/phase_transition.png")

rows = defaultdict(list)
with open(src) as fh:
    for r in csv.DictReader(fh):
        rows[int(r["n"])].append((float(r["ratio"]), float(r["p_sat"]), float(r["median_conflicts"])))

fig, ax = plt.subplots(1, 2, figsize=(13, 4.5))
for n, pts in sorted(rows.items()):
    ratio, p_sat, conflicts = zip(*pts)
    ax[0].plot(ratio, p_sat, "o-", ms=4, label=f"n = {n}")
    ax[1].semilogy(ratio, [max(c, 1) for c in conflicts], "o-", ms=4, label=f"n = {n}")
for a in ax:
    a.axvline(4.26, ls="--", color="k", lw=0.8)
    a.set_xlabel("clauses / variables")
    a.legend()
ax[0].set(ylabel="fraction satisfiable", title="A sharp transition near 4.26")
ax[1].set(ylabel="median conflicts (log)", title="Easy - hard - easy: effort peaks at the transition")
fig.tight_layout()
out.parent.mkdir(parents=True, exist_ok=True)
fig.savefig(out, dpi=110)
print(f"wrote {out}")
