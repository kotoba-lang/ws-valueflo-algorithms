#!/usr/bin/env python3
"""Measurements for the wiki-valueflow-crawl ECON job.

Decision-free. Runs the econ valueflow analysis (valueflow_econ.cljs) via kbb.
Prints SCANNED + MEASURE lines, appends an immutable ledger row.

Runs monthly (CPI) / quarterly (QCEW): these sources change on a month/quarter
cadence. Daily pings would be wasted tokens.
"""
import json
import os
import subprocess
import sys
from datetime import datetime, timezone

VFA = os.environ.get(
    "VALUE_FLO_WORKTREE",
    os.path.expanduser("~/github/com-junkawasaki/orgs/kotoba-lang/ws-valueflo-algorithms"))
VOCAB = os.environ.get(
    "VALUE_FLO_VOCAB",
    os.path.expanduser("~/github/com-junkawasaki/orgs/kotoba-lang/ws-valueflo-vocabulary"))
# kbb, not nbb: the algorithms now require kotoba.lang.coll (.cljk), which nbb
# cannot resolve ("Could not find namespace: kotoba.lang.coll", 2026-09-26).
KBB = os.environ.get("VALUE_FLO_KBB", "/opt/homebrew/bin/kbb")
HERE = os.path.dirname(os.path.abspath(__file__))
LEDGER = os.path.join(HERE, "..", "workspace", "valueflow-ledger.jsonl")
SRC = os.path.join(HERE, "..", "workspace", "valueflow_econ.cljs")


def refuse(why):
    print("REFUSED — no econ valueflow analysis was measured this run.")
    print(why)
    print()
    print("Do not propose anything. Report this refusal and stop.")
    sys.exit(0)


def main():
    for p in (VFA, VOCAB):
        if not os.path.isdir(p):
            refuse(f"missing checkout: {p}")
    if not os.path.isfile(SRC):
        refuse(f"missing analysis source: {SRC}")
    proc = subprocess.run(
        [KBB, "--backend", "sci", "--classpath", f"{VFA}/src:{VOCAB}/src", SRC],
        capture_output=True, text=True, timeout=180)
    if proc.returncode != 0:
        refuse("the analysis script exited %d:\n%s"
               % (proc.returncode, (proc.stderr.strip() or proc.stdout.strip())[:800]))
    out = proc.stdout.strip()
    if "SCANNED\t" not in out:
        refuse("the analysis produced no SCANNED line")
    row = {"measured_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
           "job": "econ"}
    with open(LEDGER, "a") as f:
        f.write(json.dumps(row) + "\n")
    print(f"worktree\t{VFA}")
    print(f"ledger\t{LEDGER}")
    print(out)


if __name__ == "__main__":
    main()