---
title: project_query_cache_readert_passthrough
description: Query cache (cached/CacheKey) is a no-op across every runReaderT(modifiedGame) boundary; scoped cache + CacheReaderT delegation fixes window-build timeouts (issue
---

The engine query cache (`cached :: CacheKey v -> m v -> m v`, `Classes/HasGame.hs`) only
memoizes when `m` is `GameT` (IORef cache) or a scoped State cache (`ModifierBuilder`/`CachedT`).
`instance HasGame (ReaderT Game m)` is a **no-op passthrough** (must stay so — `ReaderT Game
Identity` is used for pure AI snapshots, `Identity` isn't `HasGame`). So the cache dies at
EVERY `getGame >>= runReaderT body . modifyGame` boundary — and those are pervasive on the
hot ability/window path: `asIfTurn`/`asActive`/`withActiveInvestigator` (`GameEnv.hs`),
`getLocationsMatching`'s `gameAllowEmptySpaces` toggle (`Game.hs`), and the top-level
`withDepthGuard` (`Helpers/Game.hs`) wrapping `PerformableAbility` (`Game.hs:1837`).

Result (issue #4985): a fast player window with 2× Eon Chart in a grid/empty-space scenario
(Lost in Time and Space) re-ran ALL grid accessibility (`AccessibleFrom`/`AccessibleTo`/
`ConnectedFrom ForMovement`, `OnLocation`, `getConnectedMatcher`) uncached, O(abilities³),
taking >30s → server `RunMessagesTimeout`. Server-side move cost ~6.7s → ~0.2s (~30×) after fix.

**Fix pattern (reuse for any window/ability-enumeration perf work):**
- Wrap the read-only pass (e.g. `getActions`) in a live scoped cache: `runCachedQueryT`
  (`CachedT` in `Modifier/Builder.hs`) — like `buildModifiers` but keeps tracing.
- For the game-swapping boundaries, run the body in `CacheReaderT`/`runCacheReaderT`
  (delegates the cache DOWN to the base monad while overriding `getGame` to the modified
  game) instead of plain `runReaderT`. `withDepthGuard`, `getLocationsMatching`'s empty-space
  else-branch now use this.
- `gameAllowEmptySpaces` toggles: `cached` namespaces keys via `NamespaceEmptyKey` so
  flag=True (`IncludeEmptySpace`) entries never collide with flag=False.
- Skip redundant `withActiveInvestigator` re-entry when already active (`Helpers/Ability.hs`
  `getCanPerformAbility`).
- `getLocationsMatching`'s `go`/`go1` generalized to `(HasGame n, Tracing n) => n` so the
  common (unchanged-game) branch runs in the live-cache monad, not a fresh passthrough ReaderT.

Also: cards using `PerformableAbility` in criteria (Eon Chart etc.) should add
`NotAbility (AbilityOnCard $ cardsAre [self...])` to prune the enumeration (semantically safe
— those abilities are Fast, never `AbilityIsAction`). Committed as "Some speed fixes for performable".

Diagnosis tool: `arkham-replay --undo N --answers a.json --metrics` — but note `ModifierBuilder`
no-ops tracing, so wrap-in-`CachedT` (which delegates tracing to `GameT`) to see spans inside
the cached pass. Look for `passesCriteria/AbilityExists` and `passesCriteria/OnLocation` still
showing high per-call cost = cache not reaching that path. Related: [[project_action_diff_snapshot]].
