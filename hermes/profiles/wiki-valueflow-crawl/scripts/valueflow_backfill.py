#!/usr/bin/env python3
"""ISIC classification backfill for the wiki-valueflow-crawl bot.

Existing GLEIF legal entities carry NO industry field. This job classifies a
batch of legal entities (name + jurisdiction + legal_form) into an ISIC section
using gpt-oss-120b direct via OpenRouter, then aggregates the classified
entities into industry x jurisdiction valueflow recipe shapes and writes them
as an EDN proposal for wiki.yataverse.com.

Decidable parts (GLEIF read, ISIC call, aggregation, EDN write) live here; the
agent's only job is to read the SCANNED/MEASURE output and propose ONE finding.

Run with a batch size. The full 127k-entity backfill fits under a 10B
token/day budget (250 tok/entity => 32M token total => under 1 day).
"""
import json
import os
import re
import subprocess
import sys
import urllib.request
from datetime import datetime, timezone

HERE = os.path.dirname(os.path.abspath(__file__))
LEDGER = os.path.join(HERE, "..", "workspace", "valueflow-ledger.jsonl")
OUT = os.path.join(HERE, "..", "workspace", "gleif-isic-backfill.edn")
PROGRESS = os.path.join(HERE, "..", "workspace", "gleif-isic-progress.jsonl")


def _load_env():
    """Read this profile's .env (runtime injects it; manual runs need it)."""
    env_path = os.path.join(HERE, "..", ".env")
    if os.path.isfile(env_path) and not os.environ.get("OPENROUTER_API_KEY"):
        for line in open(env_path):
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                k, v = line.split("=", 1)
                os.environ.setdefault(k, v.strip().strip('"').strip("'"))


_load_env()

# --- config ---
BATCH = int(os.environ.get("VF_BACKFILL_BATCH", "50"))        # entities per run
API = "https://openrouter.ai/api/v1/chat/completions"
KEY = os.environ.get("OPENROUTER_API_KEY", "")
MODEL = os.environ.get("VF_CLASSIFY_MODEL", "openai/gpt-oss-120b")

ISIC_SECTIONS = [
    "A Agriculture, forestry and fishing", "B Mining and quarrying",
    "C Manufacturing", "D Electricity, gas, steam and air conditioning supply",
    "E Water supply; sewerage, waste management", "F Construction",
    "G Wholesale and retail trade; repair of motor vehicles",
    "H Transportation and storage", "I Accommodation and food service activities",
    "J Information and communication", "K Financial and insurance activities",
    "L Real estate activities", "M Professional, scientific and technical activities",
    "N Administrative and support service activities",
    "O Public administration and defence", "P Education",
    "Q Human health and social work activities",
    "R Arts, entertainment and recreation", "S Other service activities",
]


def refuse(why):
    print("REFUSED — no backfill measurement this run.")
    print(why)
    print("Report this refusal and stop.")
    sys.exit(0)


def read_gleif_batch(limit):
    """Read the next unprocessed batch of GLEIF legal entities from R2.

    Deterministic. Skips LEIs already recorded in the progress ledger so each
    run classifies NEW entities (no re-classifying the same firm). Returns
    [] (refuse-worthy) only if everything is processed or read fails.
    """
    seen = set()
    if os.path.isfile(PROGRESS):
        for line in open(PROGRESS):
            line = line.strip()
            if line:
                try:
                    seen.add(json.loads(line).get("lei"))
                except Exception:
                    pass
    code = '''
import sys
sys.path.insert(0, 'scripts')
import datalake_catalog
cat = datalake_catalog.connect()
t = cat.load_table(('cloud_itonami','gleif_lei_joined'))
df = t.scan().to_arrow().to_pylist()
import json
print(json.dumps(df))
'''
    env = dict(os.environ)
    env["CF_CATALOG_TOKEN"] = os.popen(
        "security find-generic-password -s gftd.cf -a API_TOKEN -w 2>/dev/null").read().strip()
    r = subprocess.run(["/opt/homebrew/bin/python3", "-c", code],
                       capture_output=True, text=True,
                       cwd=os.path.expanduser("~/github/com-junkawasaki"),
                       env=env, timeout=120)
    if r.returncode != 0:
        refuse("GLEIF R2 read failed: " + r.stderr.strip()[:300])
    ents = json.loads(r.stdout.strip().split("\n")[-1])
    fresh = [e for e in ents if e.get("lei") not in seen]
    print(f"MEASURE\tbackfill.total\t{len(ents)}")
    print(f"MEASURE\tbackfill.processed-so-far\t{len(seen)}")
    print(f"MEASURE\tbackfill.pending\t{len(fresh)}")
    return fresh[:limit]


def classify(entity):
    """ISIC section for one legal entity via gpt-oss-120b. """
    name = entity.get("legal_name", "")
    juris = entity.get("jurisdiction", "")
    lf = entity.get("legal_form", "")
    prompt = (f"Classify this legal entity into exactly one ISIC section "
              f"(letter + title), based on the company name.\n"
              f"Name: {name}\nJurisdiction: {juris}\nLegal form code: {lf}\n"
              f"ISIC sections:\n" + "\n".join(ISIC_SECTIONS) +
              "\nReply with ONLY the section letter (A-S), nothing else.")
    body = json.dumps({
        "model": MODEL,
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": 512, "temperature": 0,
    }).encode()
    req = urllib.request.Request(API, data=body, headers={
        "Authorization": f"Bearer {KEY}",
        "Content-Type": "application/json",
    })
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            data = json.loads(resp.read())
            msg = data["choices"][0]["message"]
            content = (msg.get("content") or "").strip()
            if not content and msg.get("reasoning"):
                # reasoning model: the final letter appears at the tail of reasoning
                content = msg["reasoning"].strip()
            m = re.search(r"\b([A-S])\b", content)
            return m.group(1) if m else ("X:" + content[-20:])
    except Exception as e:
        return "ERR:" + repr(e)[:80]


def main():
    if not KEY:
        refuse("OPENROUTER_API_KEY not set in this profile's .env")
    ents = read_gleif_batch(BATCH)
    if not ents:
        print("SCANNED\t0")
        print("MEASURE\tbackfill.status\tALL-PROCESSED")
        print("No unprocessed GLEIF entities remain. The 4-month backfill is "
              "complete. Do not open a PR proposing re-classification.")
        with open(LEDGER, "a") as f:
            f.write(json.dumps({"measured_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
                                "job": "backfill", "status": "complete"}) + "\n")
        sys.exit(0)
    results = []
    for e in ents:
        sec = classify(e)
        results.append({"lei": e["lei"], "legal_name": e["legal_name"],
                        "jurisdiction": e["jurisdiction"], "isic": sec})
        # record progress deterministically as we go (even ERR, so a policy
        # fix can retry by clearing progress rather than a blind re-sweep)
        with open(PROGRESS, "a") as f:
            f.write(json.dumps({"lei": e["lei"], "isic": sec,
                                "at": datetime.now(timezone.utc).isoformat(timespec="seconds")}) + "\n")
    # aggregate -> industry x jurisdiction recipe seed
    from collections import Counter
    agg = Counter((r["isic"], r["jurisdiction"]) for r in results)
    # write EDN proposal
    with open(OUT, "w") as f:
        f.write(";; gleif-isic backfill proposal (generated %s)\n[\n"
                % datetime.now(timezone.utc).isoformat(timespec="seconds"))
        lines = []
        for (isic, juris), n in agg.most_common():
            lines.append(f'  {{:isic "{isic}" :jurisdiction "{juris}" :entities {n}}}')
        f.write(",\n".join(lines) + "\n]\n")
    row = {"measured_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
           "job": "backfill", "batch": len(results), "classified": sum(1 for r in results if not r["isic"].startswith("ERR"))}
    with open(LEDGER, "a") as f:
        f.write(json.dumps(row) + "\n")
    print(f"SCANNED\t{len(results)}")
    print(f"MEASURE\tbackfill.classified\t{row['classified']}")
    print(f"MEASURE\tbackfill.batch\t{len(results)}")
    print(f"MEASURE\tbackfill.proposal\t{OUT}")
    from collections import Counter as C
    for (isic, juris), n in agg.most_common(10):
        print(f"MEASURE\tbackfill.agg.{isic}/{juris}\t{n}")
    print("EDN proposal written to " + OUT)


if __name__ == "__main__":
    main()