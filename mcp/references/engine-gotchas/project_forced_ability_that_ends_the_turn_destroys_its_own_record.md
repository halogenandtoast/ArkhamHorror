---
name: project_forced_ability_that_ends_the_turn_destroys_its_own_record
description: "A Forced ability whose handler ends the turn is resolving INSIDE its own still-open window when EndTurn arrives, and EndTurn's blanket PerWindow purge deletes the use record that would stop it — so the window's trailing Do (CheckWindows ws) re-derives it and it fires forever; Frozen Spring looped unbreakably (#6018)"
metadata:
  node_type: memory
  type: project
---

`EndTurn` purges `PerWindow` use records (`isPerWindowRecord`,
`Arkham/Investigator/Runner.hs`) on the premise that *no window spans a turn boundary*.
That premise is false for one shape: **a Forced ability that ends the turn**. It is
resolving inside its own window when `EndTurn` arrives, so the purge destroys the very
record that stops it being re-derived.

The loop needs three facts to line up:

1. **A Forced ability's default limit is a `PerWindow` bucket.** `forcedLimit`
   (`Arkham/Ability.hs`) maps most windows — including `RevealLocation`, `Enters #after`,
   `EnemyAttacks`, `TurnBegins`, `AttemptExplore` — to `GroupLimit PerWindow 1`. (A
   `SkillTestResult` window maps to `GroupLimit PerTest 1` instead, which `EndTurn` does
   **not** touch, so those cards were never exposed.)
2. **`WindowAsk` always queues a trailing re-check.** `Arkham/Game/Runner.hs` pushes
   `Do (CheckWindows ws)` after the ask, which re-derives the window's abilities from
   scratch.
3. **`EndTurn` ran in between.** `ChooseEndTurn` → `Do (ChooseEndTurn)` →
   `resolveWithWindow (EndTurn iid) (TurnEnds iid)`. The record is gone by the time the
   re-check runs, `getCanAffordUseWith` finds `countInWs = 0`, and the ability is offered
   again — which ends the turn again, which drops the record again.

**Why:** issue #6018, a regression from `ec62fb1646` (the #5999 Safeguard (2) fix, which
introduced the blanket purge). Frozen Spring (`02288`, Where Doom Awaits) prints
*"**Forced** – After you reveal Frozen Spring: Lose the remainder of your actions and
immediately end your turn"*, so its handler is `setActions iid … 0` + `endYourTurn iid`.
The export's trace was eight identical cycles —
`UseAbility 02288 → SetActions 0 → ChooseEndTurn → EndTurn → CheckWindows
RevealLocationByGroup → the same WindowAsk` — with the game dragged through
Investigation → Enemy → Upkeep while still re-offering, and no way for the player out.

The fix narrows the purge rather than removing it: `expiredPerWindowRecord` keeps a
`PerWindow` record whose window the queue **still owes a `Do (CheckWindows ws)` for**,
reusing `queuedWindowChecks` — the same guard, for the same reason, that
`EndCheckWindow` already applies (#5772). It is self-cleaning: when the window finally
closes, `EndCheckWindow` runs with depth decremented to 0 against `usedDepth = 1`, so
`0 >= 1` is false and `stillChecking` is false, and the record goes. #5999 stays fixed
because Safeguard (2)'s depth-0 player-window records have no pending check for
`When FastPlayerWindow` / `When TurnBegins <iid>` at a turn boundary.

**`onlyOnce` is NOT a valid patch for this**, however tempting. It is
`groupLimit PerGame` (`Arkham/Ability.hs`), and `UnrevealLocation` exists — The Spaces
Between (`Arkham/Treachery/Cards/TheDunwichLegacy/WhereDoomAwaits/SpacesBetween.hs`)
turns over every non-Sentinel-Hill location **in that very scenario**, after which the
Diverging Paths are re-revealed and the Forced ability must fire again. A `PerGame`
bucket would have silenced the card for the rest of the scenario. Same trap for any
"once per X" invented to stop a loop: the limit is not on the card, so it will be wrong
in whatever situation the engine can legitimately repeat the window.

**How to apply:** when a Forced ability re-offers itself without end, check whether its
handler ends the turn (or otherwise crosses a boundary that purges use records) before
suspecting the window matcher — the matcher is usually fine. The signature in an export
is an empty `usedAbilities` on the acting investigator beside a pending `AbilityLabel`
for a `GroupLimit PerWindow 1` ability, plus several identical `Do (CheckWindows …)`
copies in `choiceMessages` (one per loop iteration). Cards with the same shape that this
also fixed: Overgrown Ruins (#6027), Exhibit Hall (Athabaskan Exhibit), Rope Bridge, Apex
Strangleweed, Pocket Portal (ability **1**, `AssetEntersPlay #after`), Mt. Ngranek (whose
end-turn rides its ability's `FailedThisSkillTest`, still ahead of the window's trailing
check) and Bathroom (Return to the Gathering, `RevealChaosToken #after`).

**Not** Classroom_227, Cafeteria or Ritual Circle 224: those end the turn from a
**Haunted** ability, and `getResolvableHauntedAbilities` collects those at one point in
`ActiveCost` / `SkillTest.Runner` instead of deriving them from a window, so there is no
trailing re-check to re-offer them. Same for every action ability and event that ends a
turn (Rite of Seeking, Flurry of Blows (5), Let God Sort Them Out).

**Every other use-record bucket was audited (2026-11-21) and is not exposed.** The loop
needs three things at once: a bucket some boundary message purges unconditionally, an
ability whose own handler causes that boundary, and a *window-derived* ability, so that a
trailing `Do (CheckWindows ws)` exists to re-derive it.

- `PerTurn`, purged unconditionally at `EndTurn`: no card that ends a turn carries an
  explicit `PerTurn` limit -- every one of them is a default Forced, i.e. `PerWindow`.
- `PerPhase` (`EndInvestigation`/`EndEnemy`/`EndUpkeep`/`EndMythos`/`EndSetup`) and
  `PerRound` (`EndRound`): no card handler pushes a phase or round end at all; only
  scenario and phase code does.
- `PerTest`/`PerTestOrAbility` (`SkillTestEnds`): no card pushes `SkillTestEnds`, and a
  result window's trailing check runs ahead of the test teardown regardless. Subterranean
  Swamp and Museum of Egyptian Antiquities do end the turn from a `SkillTestResult #after`
  forced, but that is a `GroupLimit PerTest 1`, which `EndTurn` never touched.
- `PerDepthLevel` (`ScenarioCountIncrementBy CurrentDepth`): neither Bright Canyon's nor
  Dark Young Host's handler changes the depth.
- `PerSpawn` (`EnemySpawned`, scoped to that enemy) and `PerMove`/`PerMovement`
  (`ResolvedMovement`, scoped to that movement): no card re-spawns the same enemy from
  inside its own spawn window, and an `Enters` forced is stamped `PerMovement` with the
  live movement id, which a nested move's `ResolvedMovement` does not match.

Worth remembering about the guard itself: `queuedWindowChecks` does not unwrap
`PhaseStep`, so the phase-boundary `CheckWindows [mkWhen FastPlayerWindow]` blocks are
invisible to it -- which is what stops them preserving a depth-0 record (#5999's immortal
shape) across a turn end. The bare `FastPlayerWindow` checks, in `SkillTest/Runner`
(`StartSkillTest`, `BeginSkillTestAfterFast`), are consumed during test setup, before any
Forced can end a turn.

**Regression specs** (all three fail on the pre-fix blanket purge, verified by reverting
`expiredPerWindowRecord` to `pure isPerWindowRecord`):
`tests/Arkham/Location/Cards/TheDunwichLegacy/WhereDoomAwaits/FrozenSpringSpec.hs` and
`tests/Arkham/Location/Cards/TheForgottenAge/Rainforest/OvergrownRuinsSpec.hs` cover both
windows in the bucket plus the re-reveal that must fire again, and
`tests/Arkham/Asset/Assets/Safeguard2Spec.hs` holds the #5999 side -- "can be armed again
even with an unrelated window check still pending", which an unscoped guard fails because
`resolveWithWindow` always leaves the `#after TurnEnds` check queued behind `EndTurn`. They
answer the forced window with `resolveForcedAbility` (`TestImport.New`), which pushes the
server's `ClearUI` ahead of the answer; without it the answered question lingers in
`gameQuestion` and a re-offer cannot be told from the offer just taken.

Related: [[project_perwindow_record_at_depth_zero_is_immortal]],
[[project_materialised_initiations_outlive_their_window_match]],
[[project_run_messages_timeout_is_a_looping_forced]].
