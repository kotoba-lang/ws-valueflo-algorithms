#!/usr/bin/env python3
"""Measurements for the wiki-valueflow-crawl bot.

Decision-free. Runs the actual valueflow algorithms (ws-valueflo-algorithms +
ws-valueflo-vocabulary via kbb) against real economic wage observations
(BLS QCEW NAICS industry weekly wage), prints a SCANNED line + the analysis
output, and appends an immutable row to the valueflow ledger. REFUSED banner
on any failure so the bot is told it is blind, never that analysis is done.

This is an ANALYSIS bot, not a source-crawl bot: there is no verify_source
proposal gate. The agent's only job is to read this measurement and propose
ONE valueflow analysis finding for wiki.yataverse.com. publish is NOT granted.
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
LEDGER = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "workspace", "valueflow-ledger.jsonl")
POC = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "workspace", "valueflow_analysis.cljs")


def refuse(why):
    print("REFUSED — no valueflow analysis was measured this run.")
    print(why)
    print()
    print("Do not propose anything. An analysis built on an unmeasurable tree "
          "is an analysis built on nothing. Report this refusal and stop.")
    sys.exit(0)


def main():
    for p in (VFA, VOCAB):
        if not os.path.isdir(p):
            refuse(f"missing checkout: {p}")
    if not os.path.isfile(POC):
        refuse(f"missing analysis source: {POC}")

    proc = subprocess.run(
        [KBB, "--backend", "sci", "--classpath", f"{VFA}/src:{VOCAB}/src", POC],
        capture_output=True, text=True, timeout=180)
    if proc.returncode != 0:
        refuse("the analysis script exited %d:\n%s"
               % (proc.returncode, (proc.stderr.strip() or proc.stdout.strip())[:800]))

    stdout = proc.stdout.strip()
    if stdout == "" or "SCANNED\t" not in stdout:
        refuse("the analysis produced no SCANNED line")

    # append immutable ledger row (idempotency via at-timestamp + head digest)
    now = datetime.now(timezone.utc).isoformat(timespec="seconds")
    row = {"measured_at": now, "head": stdout.splitlines()[0].split("\t")[-1]}
    with open(LEDGER, "a") as f:
        f.write(json.dumps(row) + "\n")

    print(f"worktree\t{VFA}")
    print(f"ledger\t{LEDGER}")
    print(stdout)


if __name__ == "__main__":
    main()