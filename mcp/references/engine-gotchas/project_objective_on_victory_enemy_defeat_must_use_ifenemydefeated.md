# An Objective on a victory enemy's defeat MUST use `IfEnemyDefeated`, or that enemy's XP is lost

`closeDefeat` pushes `[EnemyDefeated #after] <> disposal <> [IfEnemyDefeated #after]`
(see [[project_ifenemydefeated_resolves_after_disposal]]), so an act Objective written as

```haskell
mkAbility a 1 $ Objective $ forced $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.boss)
```

fires **while the boss's `AddToVictory` is still sitting in the queue behind it**. Everything
the Objective does — `advanceVia`, `AdvanceAct`, `push R4` — is pushed in front of that
disposal, and resolution XP is computed **synchronously**: `earnXp`/`allGainXp` →
`getInitialVictory`/`generateXpReport` read `ScenarioVictoryDisplay` the moment the
`ScenarioResolution` handler runs (`Helpers/Xp.hs`). The snapshot is therefore taken
before the enemy is in the display, and its `cdVictoryPoints` are silently dropped.

Nothing errors. The breakdown the player sees is complete and plausible — every *other*
victory card is there — so the loss only surfaces as "the boss gave no XP".

**Use `ifEnemyDefeated` / `ifEnemyDefeatedMatch` (`Act/Import/Lifted.hs`), which are
`IfEnemyDefeated #after Anyone ByAny`.** The `#after` arm widens the enemy match through
`DefeatedEnemy` (`Helpers/Window.hs`), so `enemyIs` and trait matchers still match the
out-of-play enemy. This is already what 10+ official acts do (`TheHiveMind`,
`HarvesterOfWoe`, `FalseLight`, `FaceToCarapace`, `Bloodbath`, …).

## When the pre-disposal window is still right

- The Objective **replaces** the disposal → `EnemyDefeated #when` + `insteadOfDiscarding`.
- The Objective reads something disposal clears. `EnteringTheUnderworld` V1/V2 keep
  `EnemyDefeated #after` because victory removal clears the enemy's clues, and
  `TheBindingRite`/`TheBrokenRite` keep it for the Spectral Watcher. All four say so in a
  comment — add one if you need the early window.
- `TheHourOfJudgment` instead calls `addToVictory_` on Neith by hand inside the ability
  before advancing. That works (the scenario runner `nub`s the display, so the later
  disposal is a no-op) but it is a workaround, not the pattern to copy.

Not affected: an Objective whose enemy has no `cdVictoryPoints`, or `Just 0`
(TracesToNowhere's bosses), or one that only places doom (`PlayingCards`).

Bug this explains: #5975 — Dark Matter's *In the Shadow of Earth* act 3 "The Shadow of
Earth"; The Entity's 2 victory points never reached the scenario total on resolution 4.
The export's step 2735 shows `CheckWindows [#after ActAdvance "c:dark-matter:118"]` queued
ahead of `AddToVictory (EnemyTarget …)`. Fixed alongside the same latent bug in Sleepy
Hollow's *Banishing the Spectre* (The Headless Horseman, 2 VP) and And Then There Were
None's *Best Laid Plans* (Culprit, 1 VP).

Related: [[project_ifenemydefeated_resolves_after_disposal]],
[[project_addtovictory_leaveplay_window_ordering]], [[project_leave_play_tombstones]],
[[project_setglobal_priority_victory_clearqueue]].
