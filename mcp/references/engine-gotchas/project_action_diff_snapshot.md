---
title: project-action-diff-snapshot
description: gameActionDiff is now a single lazy revert diff vs a transient snapshot; arkham-replay gained --simulate-server / --bench-action-diff / partial --undo replay
---

In-action undo bookkeeping (June 2026): `gameActionDiff` holds ONE lazy revert
patch against `gameActionSnapshot` (a `Transient Game` field — never serialized,
omitted by the hand-written `ToJSON Game` in `Arkham/Game/Json.hs`). It is
rebuilt after mid-action loads by folding saved patches (also covers legacy
multi-patch saves). Do not reintroduce per-message `diff new old` consing in
`handleActionDiff` — that cost up to ~10.5s per save on large games (918
messages) vs ~12ms now.

**Why:** every mid-action save forced M full-game serializations + tree diffs.

**How to apply:** any new revert/undo bookkeeping should follow the same
pattern: keep a runtime-only snapshot (`Transient` wrapper keeps JSON shape
unchanged) + one lazy diff, never per-message materialized diffs.

Benchmark tooling in `arkham-replay`:
- `--replay-all --undo N` replays only the last N steps (full undo to step 0
  crashes on most exports — campaign-level state without a scenario).
- `--simulate-server` mirrors updateGame's per-answer JSON work (forceActionDiff,
  diffDown, encodeGame, parseGame, encodePublicGame) as server/* metric spans.
- `--bench-action-diff K` measures the save cost after K in-action messages.
- `--replay-all` does NOT re-apply answers (exports store only leftover queues
  + revert patches), so replay trajectories diverge from the original session;
  final states are still deterministic per binary — compare old-vs-new binary
  finals to prove behavior parity (ignore gameActionDiff).

Remaining per-answer costs in updateGame (follow-up candidates, ~50ms total on
large games): full Game re-parse from the row (~19ms — needs a step-keyed cache
with invalidation in Undo/Old/Debug/Decks/PendingGames writers), diffDown +
replace double-serialization (~11ms — share toJSON via ArkhamGameRaw), and the
PublicGame broadcast encode (~12ms).
