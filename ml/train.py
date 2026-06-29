#!/usr/bin/env python3
"""Train a learning-to-rank model that imitates human Arkham Horror play.

Input: JSONL produced by the `arkham-extract` tool (one row per choice per
decision), with the schema:

    {
      "game_id": "<uuid>",
      "step": <int>,
      "player_id": "<uuid>",
      "group_id": "<game_id>:<step>",   # one ranking group per decision
      "chosen": 0 | 1,                    # 1 for the choice the human took
      "features": { "<67 choiceFeatures keys>": <float>, ... },
      "breakdown": { "<10 scoreBreakdown keys>": <int>, ... },
      "outcome":  "win" | "loss" | "partial" | "unknown"
    }

The model is a LightGBM `lambdarank` ranker over the per-(situation, choice)
features. At inference the Haskell engine computes the SAME `choiceFeatures`
and the distilled model re-ranks the choices (replacing the generic
`bestByScore` argmax). See ml/README.md.

Key practices baked in:
  * split by GAME, never by row (rows in a game are correlated -> leakage).
  * top-1 human-move accuracy is the primary metric, compared against the
    heuristic baseline (argmax of the summed `breakdown`, i.e. today's engine).
  * optional outcome-weighting toward winning games (--outcome-weighting).
  * a regularized linear ranker is trained alongside so we can decide whether
    linear-into-scoreChoice distillation is competitive with the GBDT.
  * the feature column ORDER is derived from the data, locked, and saved to
    feature_order.json so the Haskell distillation uses the identical order.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
from collections import defaultdict

import numpy as np


# --------------------------------------------------------------------------
# Data loading
# --------------------------------------------------------------------------
def iter_rows(paths):
    for path in paths:
        with open(path, "r") as fh:
            for line in fh:
                line = line.strip()
                if line:
                    yield json.loads(line)


def load_dataset(paths):
    """Load JSONL into per-group lists. Returns (groups, feature_keys).

    groups: dict group_id -> {
        "game_id", "outcome",
        "rows": [ {"chosen", "feat": {k:v}, "breakdown": {k:v}} ... ]
    }
    """
    groups = {}
    feature_keys = set()
    n = 0
    for r in iter_rows(paths):
        gid = r["group_id"]
        g = groups.get(gid)
        if g is None:
            g = {"game_id": r.get("game_id", gid.split(":")[0]),
                 "outcome": r.get("outcome", "unknown"),
                 "rows": []}
            groups[gid] = g
        feat = r["features"]
        feature_keys.update(feat.keys())
        g["rows"].append({"chosen": int(r["chosen"]),
                          "feat": feat,
                          "breakdown": r.get("breakdown", {})})
        n += 1
    # Lock a stable column order: situation block then choice block, alpha within.
    fk = sorted(feature_keys, key=lambda k: (0 if k.startswith("sit.") else 1, k))
    print(f"loaded {n} rows across {len(groups)} decisions, {len(fk)} features")
    return groups, fk


def materialize(groups, feature_keys, outcome_weighting):
    """Flatten groups into LightGBM ranking arrays, keyed by game for splitting."""
    OUTCOME_W = {"win": 1.0, "partial": 0.6, "unknown": 0.5, "loss": 0.25}
    by_game = defaultdict(list)  # game_id -> list of (X_rows, y_rows, w, breakdown_rows)
    for gid, g in groups.items():
        rows = g["rows"]
        if len(rows) < 2:
            continue  # a 1-choice "decision" carries no ranking signal
        if not any(r["chosen"] for r in rows):
            continue  # label recovery failed for this group; skip
        X = np.array([[float(r["feat"].get(k, 0.0)) for k in feature_keys] for r in rows],
                     dtype=np.float32)
        y = np.array([r["chosen"] for r in rows], dtype=np.int32)
        bd = np.array([sum(r["breakdown"].values()) if r["breakdown"] else 0.0 for r in rows],
                      dtype=np.float32)
        w = OUTCOME_W.get(g["outcome"], 0.5) if outcome_weighting else 1.0
        by_game[g["game_id"]].append((X, y, w, bd))
    return by_game


def assemble(by_game, game_ids):
    """Concatenate selected games into (X, y, group_sizes, weights, heuristic_scores)."""
    Xs, ys, groups, ws, heur = [], [], [], [], []
    for gid in game_ids:
        for (X, y, w, bd) in by_game[gid]:
            Xs.append(X)
            ys.append(y)
            groups.append(len(y))
            ws.append(w)
            heur.append(bd)
    if not Xs:
        return (np.zeros((0, 0), np.float32), np.zeros(0, np.int32), [], [], [])
    return (np.concatenate(Xs), np.concatenate(ys), groups, ws, heur)


# --------------------------------------------------------------------------
# Metrics
# --------------------------------------------------------------------------
def grouped(scores, y, group_sizes):
    i = 0
    for n in group_sizes:
        yield scores[i:i + n], y[i:i + n]
        i += n


def ranking_metrics(scores, y, group_sizes):
    top1 = top3 = mrr = total = 0
    for s, yy in grouped(scores, y, group_sizes):
        chosen = int(np.argmax(yy))           # exactly one relevance-1 per group
        order = np.argsort(-s)                 # model's ranking (desc)
        rank = int(np.where(order == chosen)[0][0])
        top1 += rank == 0
        top3 += rank < 3
        mrr += 1.0 / (rank + 1)
        total += 1
    if total == 0:
        return {"top1": 0.0, "top3": 0.0, "mrr": 0.0, "n": 0}
    return {"top1": top1 / total, "top3": top3 / total, "mrr": mrr / total, "n": total}


# --------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", nargs="+", required=True,
                    help="JSONL file(s) or globs from arkham-extract")
    ap.add_argument("--out", default="ml/model", help="output dir for model + reports")
    ap.add_argument("--outcome-weighting", action="store_true",
                    help="weight decisions by eventual game outcome (favor wins)")
    ap.add_argument("--test-frac", type=float, default=0.15)
    ap.add_argument("--num-rounds", type=int, default=800)
    ap.add_argument("--seed", type=int, default=17)
    args = ap.parse_args()

    paths = [p for pat in args.data for p in glob.glob(pat)] or args.data
    groups, feature_keys = load_dataset(paths)
    by_game = materialize(groups, feature_keys, args.outcome_weighting)

    game_ids = sorted(by_game.keys())
    rng = np.random.default_rng(args.seed)
    rng.shuffle(game_ids)
    cut = int(len(game_ids) * (1 - args.test_frac))
    train_games, test_games = game_ids[:cut], game_ids[cut:]
    print(f"{len(train_games)} train games / {len(test_games)} test games")

    Xtr, ytr, gtr, wtr, _ = assemble(by_game, train_games)
    Xte, yte, gte, _, heur_te = assemble(by_game, test_games)

    # ---- heuristic baseline (today's engine: argmax of summed breakdown) ----
    heur_flat = np.concatenate(heur_te) if len(heur_te) else np.zeros(0)
    base = ranking_metrics(heur_flat, yte, gte)
    print(f"\nHEURISTIC baseline (test): top1={base['top1']:.3f} "
          f"top3={base['top3']:.3f} mrr={base['mrr']:.3f} n={base['n']}")

    # ---- LightGBM lambdarank ----
    import lightgbm as lgb
    train_w = np.repeat(wtr, gtr) if args.outcome_weighting else None
    dtrain = lgb.Dataset(Xtr, label=ytr, group=gtr, weight=train_w,
                         feature_name=feature_keys)
    params = dict(objective="lambdarank", metric="ndcg", ndcg_eval_at=[1, 3],
                  learning_rate=0.05, num_leaves=63, min_data_in_leaf=200,
                  feature_fraction=0.8, bagging_fraction=0.8, bagging_freq=1,
                  lambda_l2=1.0, max_position=10, seed=args.seed, verbose=-1)
    gbm = lgb.train(params, dtrain, num_boost_round=args.num_rounds)

    gbm_metrics = ranking_metrics(gbm.predict(Xte), yte, gte)
    print(f"LightGBM (test):           top1={gbm_metrics['top1']:.3f} "
          f"top3={gbm_metrics['top3']:.3f} mrr={gbm_metrics['mrr']:.3f}")

    # ---- linear ranker (distillation feasibility: linear-into-scoreChoice) ----
    from sklearn.linear_model import LogisticRegression
    mu, sd = Xtr.mean(0), Xtr.std(0) + 1e-6
    lin = LogisticRegression(max_iter=2000, C=1.0)
    lin.fit((Xtr - mu) / sd, ytr)  # pointwise proxy; ranking via the learned weights
    lin_scores = ((Xte - mu) / sd) @ lin.coef_[0]
    lin_metrics = ranking_metrics(lin_scores, yte, gte)
    print(f"Linear (test):             top1={lin_metrics['top1']:.3f} "
          f"top3={lin_metrics['top3']:.3f} mrr={lin_metrics['mrr']:.3f}")

    # ---- export ----
    os.makedirs(args.out, exist_ok=True)
    gbm.save_model(os.path.join(args.out, "lgbm.txt"))
    with open(os.path.join(args.out, "feature_order.json"), "w") as fh:
        json.dump(feature_keys, fh, indent=2)
    imp = sorted(zip(feature_keys, gbm.feature_importance(importance_type="gain")),
                 key=lambda kv: -kv[1])
    with open(os.path.join(args.out, "report.json"), "w") as fh:
        json.dump({"baseline": base, "lgbm": gbm_metrics, "linear": lin_metrics,
                   "n_train_games": len(train_games), "n_test_games": len(test_games),
                   "top_features": imp[:25],
                   "linear_coef": dict(zip(feature_keys, lin.coef_[0].tolist())),
                   "linear_mu": dict(zip(feature_keys, mu.tolist())),
                   "linear_sd": dict(zip(feature_keys, sd.tolist()))}, fh, indent=2)
    print(f"\nwrote model + report to {args.out}/")
    print("top features by gain:")
    for k, v in imp[:15]:
        print(f"  {v:12.1f}  {k}")


if __name__ == "__main__":
    main()
