# Leave-play tombstones — the one way to read an entity back after it leaves

`gameTombstones :: Entities` (`Arkham/Game/Base.hs`) holds **frozen copies of entities taken on
their way out of play, before the removal blanks them**. It answers exactly one question: *what
was this when it left?* It covers **assets and enemies**.

## Why a snapshot and not a reordering

`RemovedFromPlay` blanks an asset's placement to `OutOfPlay RemovedZone` **in place** — the entity
is not deleted at that moment, its state is destroyed. Enemies are worse: `Do (Defeated …)` clears
keys, `Do (AddToVictory …)` clears every token, `RemoveEnemy` blanks the placement, and
`clearRemovedEntities` drops the entity entirely at the next `ResolvedAbility` or `BeginTurn`. So a
`#after` reaction using `AssetAt YourLocation` / `EnemyAt YourLocation` had nothing left to resolve
(Decorated Skull, #5518; Autopsy Report (3) after The Amalgam dove into the depths, #5557).

The fix is a frozen snapshot, not a reordering and not a durable ledger. The defeat message order
stays honest.

## Where it is parked

| Entity | Park point | File |
|---|---|---|
| Asset | `RemoveFromPlay (AssetSource …)` | `Game/Runner.hs`, in `runGameMessage` |
| Enemy | the **open** `Defeated (EnemyTarget …)`, and `RemoveFromPlay (EnemySource …)` | `Game/Runner.hs`, in **`runPreGameMessage`** |

Enemies must park **pre-dispatch**. `runGameMessage` runs *last* in `RunMessage Game`
(`… >>= entitiesL (runMessage msg) >>= … >>= runGameMessage msg`), by which point the Enemy
runner's own `Defeated` handler has already cleared the enemy's keys. `parkLeavingEnemyTombstone`
deliberately refuses to overwrite a defeat's snapshot with the poorer leave-play one — that is what
keeps Bounty Contracts' bounty tokens readable.

## Where it is read

- `getAssetsMatching` / `getEnemiesMatching` (`Game.hs`) splice the snapshots back in **while a
  leave-play window naming that entity is open**, scoped by `leavePlayWindowAssets` /
  `leavePlayWindowEnemies`.
- `maybeAsset` / `maybeEnemy` (`Game/Utils.hs`) fall back to tombstones as a last resort so
  `field` / `selectAgg` cannot throw `MissingEntity`.

Because visibility is gated on the window being on the stack, **cancellation is correct for free**:
cancel the defeat and the windows go with it, so the tombstone is never consulted. Replace only the
*disposal* and the tombstone is exactly right, whatever the enemy became — discard, victory
display, the depths, a deck, or straight back into play.

## Four traps, each of which cost a full rebuild to discover

1. `gameTombstones` MUST stay out of the message-dispatch chain in `Arkham.Game`'s `RunMessage Game`.
   `gameActionRemovedEntities` IS in it (`>>= actionRemovedEntitiesL (runMessage msg)`), so
   `RemovedFromPlay` blanks parked copies there too — it is a mutable in-flight store, never a
   tombstone.
2. Splicing the candidate *list* does nothing: matcher filters re-project by id (`AssetAt` is
   `filterM (fieldP AssetLocation …) as`). The overlay must be an ambient `Game` via `runReaderT g'`.
3. The revival is ambient for the whole window, so scope it to the window's own subject. Widening it
   surfaced ids nothing downstream could dereference, throwing `MissingEntity` from `selectAgg`.
4. Both `PublicGame` encoders run with `tombstonesL .~ mempty` — a dangling id in
   `investigator.assets` crashes the frontend at `game.assets[id]`.

## What this replaced

Nine copies of `orM [matches eid (DefeatedEnemy m), matches eid m]` in `Helpers/Window.hs`, and the
`EnemyAt` → `EnemyWasAt` `biplate` rewrite for `EnemyLeavesPlay #after`. Both are gone: an ordinary
matcher now resolves against what left. `DefeatedEnemy` is no longer the read-back mechanism — it is
a durable *"was defeated this scenario"* predicate and nothing more.

`enemyMatches` (`Helpers/Window/Enemy.hs`) is **not** an instance of this pattern and stays: it
tolerates enemies discarded mid-*evade* (Kymani), and evade windows are deliberately not tombstone
subjects — see [[project_evade_windows_name_removed_enemies]] for what a card reading an enemy id
out of one has to do about it.

Related: [[project_evade_windows_name_removed_enemies]],
[[project_ifenemydefeated_resolves_after_disposal]], [[project_removed_entities_cleared]],
[[project_addtovictory_leaveplay_window_ordering]], [[project_publicgame_toencoding_is_the_wire]],
[[project_query_cache_readert_passthrough]].
