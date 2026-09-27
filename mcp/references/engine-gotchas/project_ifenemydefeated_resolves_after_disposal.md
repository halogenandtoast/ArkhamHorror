# `IfEnemyDefeated #after` resolves AFTER disposal — and that is now safe

`Arkham.Behavior.Defeat.closeDefeat` pushes
`[EnemyDefeated #after] <> disposal <> [IfEnemyDefeated #after]` — **disposal sits between the two
windows**. So:

- `EnemyDefeated #when` / `#after` → enemy still in play, card not yet discarded.
- `IfEnemyDefeated #after` → the card is already in the discard / victory display, the entity is at
  `OutOfPlay RemovedZone` and about to be dropped by `clearRemovedEntities`.

## Reading the enemy back

**No longer needs a decorated matcher.** The enemy is revived from its tombstone for the duration of
its own defeat/leave-play windows, so `IfEnemyDefeated #after Anyone ByAny (EnemyAt YourLocation)`
resolves with a plain `EnemyAt`. See [[project_leave_play_tombstones]].

Historically this was served by `orM [matches eid (DefeatedEnemy m), matches eid m]`, repeated nine
times in `Helpers/Window.hs`. `DefeatedEnemy` read a ledger written by `Do (Defeated …)` — the very
message a card replacing its own disposal cancels — which is why Autopsy Report (3) went dead after
The Amalgam's Forced ability sent it to the depths (#5557). Those nine copies are gone.

## Replacing the disposal

A trigger that **replaces** the disposal ("place her in your hand", "shuffle it back into the deck",
"attach it to the agenda", "place it in the depths") must still use `EnemyDefeated #when … (be a)`
plus `insteadOfDiscarding` (`Arkham.Helpers.Enemy`), not `IfEnemyDefeated`. `insteadOfDiscarding`
strips the `Defeated` messages before `Do (Defeated …)` ever runs (so no `Discard`/`EntityDiscarded`
is generated and `defeatedL` never flips true), then re-pushes both `EnemyDefeated #after` and
`IfEnemyDefeated #after` so genuine "after you defeat an enemy" reactions still fire.

Reference implementations: `Exoroid`, `Wraith`, `TheAmalgam`, `InnsmouthJail`,
`TheConductorBeastFromBeyondTheGate`, `LaComtesseSubverterOfPlans`.

## The durable ledgers are a separate concern

Two stores answer *"was this enemy defeated?"* rather than *"what was it?"*:
`scenarioDefeatedEnemies` (scenario-lifetime, `Scenario/Runner.hs`) and `historyEnemiesDefeated`
(turn/phase/round, UI-facing in `HistoryPanel.vue`, `Game/Runner.hs`).

Both are written from **the same two messages** — `Do (Defeated …)` and `RecordDefeatedEnemy` — so
they cannot disagree. `Do` rather than the open `Defeated` is deliberate: it is past the point where
a `cancelEnemyDefeat` card can still call the defeat off, so a cancelled defeat leaves no false
entry (Cats of Ulthar's `notExists $ DefeatedEnemy …` guard depends on this). `insteadOfDiscarding`
then pushes `RecordDefeatedEnemy` by hand, because a replaced disposal *is* still a defeat and
Kerosene (1)'s "an enemy was defeated at your location this round" must see it.

`Arkham/Enemy/Import/Lifted.hs` notes that *"the IfEnemyDefeated window doesn't work on the enemy
itself"*; `deathRattle` exists to route a self-trigger through a standalone effect.

Bug this explains: #5529 — commit `4e290b44cf` bulk-swapped ~25 cards from `EnemyDefeated #after` to
`IfEnemyDefeated #after`; La Comtesse (52020) was the only one whose trigger replaced its own
disposal, so she went to the encounter discard instead of the defeating investigator's hand.

Related: [[project_leave_play_tombstones]], [[project_cancelenemydefeat_queue_layer]],
[[project_removed_entities_cleared]].
