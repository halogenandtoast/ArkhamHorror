# ML-driven AI (imitation learning-to-rank)

Trains a model from ~18k human-played games to drive the Arkham AI, by learning
to **rank the choices a human would pick**, reusing the heuristic engine's
feature extractor (`Arkham.Ai.Decision.choiceFeatures`) so training and
inference see identical features.

The engine's `scoreChoice` is already an argmax-of-additive-scorer; this project
replaces the hand-tuned weights with learned ones (distilled back into Haskell —
no runtime Python). The special-case handlers (commit-window loop-avoidance,
damage/slot-discard, amounts) are kept; the model only re-ranks the *generic*
`bestByScore` path.

## Pipeline

1. **Feature extractor** — `choiceFeatures` / `scoreBreakdown` in `Decision.hs`
   (67 features per (situation, choice); `scoreChoice = sum ∘ scoreBreakdown`).
2. **Extract** (`backend/arkham-api`, `arkham-extract` exe): replays the games
   in the restored `arkham_ml` Postgres, recovers each human decision, and emits
   JSONL training rows.
   ```
   stack exec arkham-extract -- --jobs 8 --out data/train.jsonl
   stack exec arkham-extract -- --limit 50 --out data/sample.jsonl   # smoke test
   ```
3. **Train** (this dir): LightGBM `lambdarank`, split by game, vs the heuristic
   baseline; also fits a linear ranker to test linear-into-`scoreChoice`.
   ```
   pip install -r ml/requirements.txt
   python ml/train.py --data 'data/train*.jsonl' --out ml/model --outcome-weighting
   ```
   Read `ml/model/report.json`: `lgbm.top1` vs `baseline.top1` (does the learned
   scorer beat the heuristic at predicting real human moves?) and `linear.top1`
   (is linear competitive → simplest distillation?).
4. **Distill** — bake the LINEAR ranker into Haskell (implemented):
   ```
   python ml/export_model.py        # report.json linear_* -> backend/arkham-api/data/ai-model.json
   ```
   `export_model.py` copies `report.json`'s `linear_coef`/`linear_mu`/`linear_sd`
   into `data/ai-model.json` (`{coef,mu,sd}`). That file is embedded at compile
   time by `Arkham.Ai.Model.learnedModel` (FileEmbed, like `ai-tags.json`) and
   scored per choice by `Arkham.Ai.Decision.learnedScore` — the standardized
   linear evaluator wired into `bestByScore` as the generic argmax. Activate by
   **rebuilding** and running with `ARKHAM_AI_USE_MODEL=1`; the placeholder empty
   model (or the flag off) keeps the hand-tuned heuristic byte-for-byte. Only the
   generic `bestByScore` ranking is replaced — the commit-window, damage,
   slot-discard, and amounts special cases are untouched. (A GBDT distillation
   would instead embed `lgbm.txt` + a tree evaluator; the linear path ships first
   if `linear.top1` is competitive in `report.json`.)
5. **Self-play gate** — headless two-AI auto-play; ship only if win-rate ≥ heuristic.

## Restoring the dump (one-time)

```
createdb arkham_ml
psql -d arkham_ml -c 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp";'
pg_restore --no-owner --no-acl --jobs=4 -d arkham_ml <dump>
```

## Notes / guards
- Split by **game**, never by row (rows in a game are correlated → leakage).
- **Top-1 human-move accuracy** is the imitation metric; **self-play win-rate**
  is the real ship gate (a model can match common moves and still lose).
- **Outcome-weighting** (`--outcome-weighting`) favors decisions from won games —
  the main lever against imitating bad human play.
- `feature_order.json` locks the column order so the Haskell distillation maps
  features → weights identically.
