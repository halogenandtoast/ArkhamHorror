#!/usr/bin/env python3
"""Distill the trained LINEAR ranker into the embedded Haskell model.

Reads the linear coefficients/standardization the trainer wrote into
`ml/model/report.json` (the `linear_coef`, `linear_mu`, `linear_sd` objects)
and writes them, renamed to the loader's schema, to

    backend/arkham-api/data/ai-model.json   ==  {"coef": {...}, "mu": {...}, "sd": {...}}

That file is embedded at compile time by `Arkham.Ai.Model.learnedModel`
(FileEmbed, exactly like `data/ai-tags.json`). After exporting you must REBUILD
the backend and run with `ARKHAM_AI_USE_MODEL=1` for the learned ranker to take
over the generic `bestByScore` argmax; an empty/absent file keeps the heuristic.

The model is the standardized linear scorer the engine evaluates per choice:

    score(choice) = sum_f ((value_f - mu_f) / sd_f) * coef_f

where value_f comes from `Arkham.Ai.Decision.choiceFeatures` (the same 67-key
extractor train.py fed on), so the keys here must match those feature names.

Usage:
    python ml/export_model.py                       # report.json -> data/ai-model.json
    python ml/export_model.py --report ml/model/report.json \
                              --out backend/arkham-api/data/ai-model.json
    python ml/export_model.py --prune                # drop coef==0 features (smaller, identical scores)
"""
from __future__ import annotations

import argparse
import json
import os

# Paths are resolved relative to the repo root (this file lives in ml/).
REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DEFAULT_REPORT = os.path.join(REPO_ROOT, "ml", "model", "report.json")
DEFAULT_OUT = os.path.join(REPO_ROOT, "backend", "arkham-api", "data", "ai-model.json")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--report", default=DEFAULT_REPORT,
                    help="trainer report.json with linear_* fields (default: ml/model/report.json)")
    ap.add_argument("--out", default=DEFAULT_OUT,
                    help="embedded model path (default: backend/arkham-api/data/ai-model.json)")
    ap.add_argument("--prune", action="store_true",
                    help="drop features whose coefficient is exactly 0 (smaller file, same scores)")
    args = ap.parse_args()

    with open(args.report, "r") as fh:
        report = json.load(fh)

    missing = [k for k in ("linear_coef", "linear_mu", "linear_sd") if k not in report]
    if missing:
        raise SystemExit(
            f"{args.report} is missing {missing}; re-run ml/train.py "
            f"(it writes the linear_* fields).")

    coef = report["linear_coef"]
    mu = report["linear_mu"]
    sd = report["linear_sd"]

    if args.prune:
        keep = {k for k, v in coef.items() if v != 0.0}
        coef = {k: coef[k] for k in keep}
        mu = {k: mu[k] for k in keep if k in mu}
        sd = {k: sd[k] for k in keep if k in sd}

    model = {"coef": coef, "mu": mu, "sd": sd}

    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out, "w") as fh:
        json.dump(model, fh, indent=2)
        fh.write("\n")

    nz = sum(1 for v in coef.values() if v != 0.0)
    print(f"wrote {args.out}: {len(coef)} coefficients ({nz} non-zero)")
    print("REBUILD the backend and set ARKHAM_AI_USE_MODEL=1 to activate the learned ranker.")


if __name__ == "__main__":
    main()
