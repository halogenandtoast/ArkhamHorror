---
name: project-test-harness-gotchas
description: "Hspec harness traps: defaults, stale questions, playerOrder, useAbility windows, damage wrappers"
metadata: 
  node_type: memory
  type: project
  originSessionId: ab435e17-57b4-48d7-a7f0-39f4c5292675
  modified: 2026-08-15T07:51:59.162Z
---

Merged index entry. Supersedes: [[project_test_harness_defaults]], [[project_test_skip_stale_question]], [[project_test_addinvestigator_playerorder]], [[project_test_useability_empty_windows]], [[project_damage_question_wrappers]].

## "gameTest's default investigator is Jenny Barnes and the default test location holds 2 clues — both silently skew assertions"

Two defaults in the Hspec harness that make "obviously correct" assertions fail:

- **`gameTest`'s investigator is Jenny Barnes (02003).** She has
  `modifySelf attrs [UpkeepResources 1]`, so `run AllDrawCardAndResource` grants
  **2** resources, not 1. Anything asserting on resource totals after upkeep must
  account for it (or use `gameTestWith` with a plain investigator).
- **`testLocation` / `testLocationWith` comes in with 2 clues.** Any "discover the
  last clue" window (`DiscoveringLastClue`) therefore does not fire after discovering
  1. Build it as `testLocationWith (revealCluesL .~ Static 1)` instead of placing
  extra clues on top.
- **Test locations start UNREVEALED.** `location` in `Location/CardDefs/Base.hs` sets
  `cdDoubleSided = True`, and `locationRevealed = not cdDoubleSided`. Any criteria
  with `RevealedLocation` then silently matches nothing, so the ability is never
  offered and `[action] <- getActionsFrom …` dies on the pattern bind. Add
  `revealedL .~ True` (in scope via `TestImport.New`).

**Why:** both produce failures that look like engine bugs — a doubled resource gain,
or a forced ability that "never triggers" (which surfaces as
`There must be only one question to use this function` with an empty question list,
since `useForcedAbility`/`useReaction` throw when no question exists).

**How to apply:** when a new spec's numbers are off by a suspicious amount, assert the
precondition (`self.resources`, `field LocationClues`) *before* the action to separate
a harness default from a real defect. See [[project_damage_question_wrappers]] and
[[project_test_skip_stale_question]].

## "Test helpers answer questions without the ClearUI the API pushes ahead of every answer, so declining a triggers window leaves the answered question in gameQuestion — `skip; assertNoReaction` silently always fails"

The real answer path (`Api/Handler/Arkham/Games/Shared.hs` ~401) enqueues **`ClearUI` ahead of the answer's messages**:

```haskell
queueRef <- newQueue ((ClearUI : syncMsgs <> messages) <> currentQueue)
```

`ClearUI` is what wipes `gameQuestion` (`Game/Runner.hs` ~184). The test harness's `chooseOptionMatching` / `chooseOnlyOption` / `skip` (TestImport.hs) just do `push (uiToRun msg) <* runMessages` — **no `ClearUI`**. That is invisible for a normal answer, because the resulting cascade pushes a new `Ask` that overwrites `gameQuestion`. It is NOT invisible for an answer that produces no new question — which is exactly what declining a triggers window does:

`WindowAsk` pushes `[Ask pid q, Do (CheckWindows ws)]` (`Game/Runner.hs` ~1770). Answering with `SkipTriggersButton` runs `SkippedWindow iid`, which sets `investigatorSkippedWindow` and gates that trailing `Do (CheckWindows ws)` (`Investigator/Runner.hs` ~2183). Correct behavior — but the *answered* question is still sitting in `gameQuestion`, so `assertNoReaction` re-reads it and reports the reaction you just declined.

This is a harness artifact, not an engine bug: reproduce it with any plain in-play [Spell] `[action]` + Sign Magick (3) — nothing to do with the card under test.

**How to apply:** when a spec needs "declining this window ends it", mirror the server by pushing `ClearUI` *before* the decline:

```haskell
skipWindow :: HasCallStack => TestAppT ()
skipWindow = push ClearUI >> skip
```

(`push` does not run messages, so `skip` still sees the live question, and its answer lands in front of the `ClearUI`.) Same trap applies to any `chooseOptionMatching` whose branch creates no follow-up question before an assertion that reads `gameQuestion`. Related: [[project_damage_question_wrappers]], [[project_test_useability_empty_windows]], [[project_nonturn_player_window_decline]].

## Test addInvestigator now appends to gamePlayerOrder so getInvestigators (inTurnOrder) sees added investigators

`getInvestigators` = `inTurnOrder (select UneliminatedInvestigator)`, and `inTurnOrder` filters to `gamePlayerOrder`. The test game starts with `gamePlayerOrder = [primaryId]` only. `addInvestigator` (TestImport.hs) originally inserted just the entity, so added investigators were invisible to `getInvestigators`/`getInvestigators`-based matchers (SearchAllInvestigators fold, NearestLocationToMost votes).

Fixed by making `addInvestigator` also append `toId investigator'` to `playerOrderL`.

**How to apply:** Do NOT manually re-append to `gamePlayerOrder` after `addInvestigator` — that double-counts the investigator (duplicate turn-order entry → duplicate votes/queries). Just call `addInvestigator`.

## Test helper useAbility passes empty windows; cards re-checking in-hand ability performability crash — activate via raw UseAbility with defaultWindows

The `useAbility` test helper (tests/TestImport/New.hs) is `run $ UseAbility (toId i) a []` — it passes an **empty** window list. Most action-ability tests work anyway because the activated ability's own performability doesn't depend on those windows.

But a card whose handler re-checks the performability of OTHER abilities against the passed windows breaks: an in-hand `[action]` needs a `DuringYourAction`/`DuringTurn` window to read as performable, so with `[]` the re-check finds nothing and a `chooseOne` over the (now empty) list throws `No messages for chooseOne`. Hit this with True Magick: Reworking Reality (`UseCardAbility … 1 ws _` filters in-hand spell abilities by `getCanPerformAbility iid ws`).

**How to apply:** when activating such an ability in a spec, bypass `useAbility` and push real windows yourself:
`run $ UseAbility (toId self) ability (defaultWindows $ toId self)` (import `defaultWindows` from `Arkham.Window` — TestImport only re-exports `Window(..)` + a few `WindowType`s, not `defaultWindows`). The criterion that GATES the action (e.g. `HasTrueMagick`) checks with the same `getCanPerformAbility iid windows'`, so passing `defaultWindows` keeps offer-time and resolve-time in lockstep. See [[project_truemagick_signmagick_ability_exposure]].

## "Damage/horror assignment questions are wrapped (QuestionWithSource+QuestionLabel); test helpers must strip them, and a lone AoO is still a chooseOneAtATime to resolve first"

The "Label damage assignment with source and remaining totals" change (Damage.hs `assignDamageDivided`) now pushes the damage/horror assignment `ChooseOne` wrapped as `QuestionWithSource <source>` (board source-highlight) → `QuestionLabel "Assign N damage"` (totals header) → `ChooseOne [ComponentLabel ...]`. `DamageLabel`/`AssetDamageLabel` are pattern synonyms for `ComponentLabel (InvestigatorComponent/AssetComponent _ DamageToken)`.

Test harness consequence: helpers that pattern-match `case question of ChooseOne ...` stopped seeing the choices (errored "unsupported questions type" or silently got 0 damage). Fix is `stripQuestionWrappers` (in `tests/TestImport.hs`, re-exported via TestImport.New) which peels `QuestionLabel`/`QuestionWithSource`/`PayCostQuestion`; applied across `applyAllDamage`, `applyAllHorror`, `assert*`, `chooseOnlyOption`/`chooseFirstOption`/`chooseOptionMatching`, and `chooseOptionAcrossQuestions`' `findIn`.

Separate gotcha: an attack of opportunity (even a single enemy) is ALWAYS presented as `chooseOneAtATime` (Game/Runner.hs `EnemyAttacks -> chooseOneAtATime`). A test that provokes an AoO must resolve it (e.g. `chooseOnlyOption`) BEFORE `applyAllDamage` — the attack only deals/assigns damage once executed. `applyAllDamage` only drains `ChooseOne`, not the attack-resolution `ChooseOneAtATime`.

Relates to [[project_aoo_gated_at_callsite]] and [[project_simultaneous_damage_window_targets]].

## "Entities built in the test body have NO modifiers until the next message — `runMessages` preloads AFTER `runMessage`, so the first message reads a stale `gameModifiers`"

`gameTest`/`gameTestWith`/`scenarioTest*` call `overGameM preloadModifiers` **once, before the body runs** (`tests/TestImport.hs` ~822/835/860). Every `test*` builder (`testLocationWithDef`, `testEnemy`, `testAsset`, …) then inserts its entity with a raw `overGame`, which does not preload.

`Game.runMessages` preloads in the wrong order for this: it runs `preloadEntities` → `runPreGameMessage` → **`runMessage msg >=> preloadModifiers`** (`library/Arkham/Game.hs` ~6738-6741). So the *first* message after the body creates an entity sees a `gameModifiers` map that predates it. Any handler doing `mods <- getModifiers a` reads `[]`.

Symptom: a `HasModifiersFor`/`modifySelf` modifier "does nothing" on the first message but works on every later one. Hit this on UndergroundRiverSpec — `SetFloodLevel`'s `CannotBeFullyFlooded` clamp (`Location/Runner.hs` ~491) silently didn't apply, which looks exactly like the engine bug the spec was regression-testing.

**How to apply:** after building an entity whose own modifiers matter to the very next message, `tick` (= `run Noop`; `Noop` is not in `shouldPreloadModifiers`' False list, so it preloads). Fold it into the builder:

```haskell
undergroundRiver :: TestAppT Location
undergroundRiver = testLocationWithDef Locations.undergroundRiver (revealedL .~ True) <* tick
```

Same reason `TheBlackCat5Spec`'s `asScenario` ticks after `overTest`. This is a harness artifact — real games always have a prior message's preload. See [[project_test_harness_gotchas]] above for the other builder defaults.

## "Test games always have a turn player, so `DuringTurn You` criteria can never fail in a spec — clear `turnPlayerInvestigatorIdL` to cover the negative case"

`newGame` (tests/TestImport.hs ~881) hardcodes `gameTurnPlayerInvestigatorId = Just investigatorId`
even though `gamePhase = CampaignPhase`. `Criteria.DuringTurn` resolves to
`selectAny (TurnInvestigator <> who)` (`Helpers/Criteria.hs` ~492), so **every** spec runs as if it
were the investigator's turn. A card whose only bug is a missing "during your turn" restriction will
therefore pass its happy-path spec both before and after the fix.

**How to apply:** for the negative half of such a spec, set the field directly —
`overTest $ turnPlayerInvestigatorIdL .~ Nothing` (the lens comes from `Arkham.Game.Base`, which
`TestImport` re-exports via `Arkham.Game as X`). That reproduces the Mythos/Enemy/Upkeep-phase
state exactly; the engine clears the same field in `After (EndTurn _)` and `EndInvestigation`
(`Game/Runner.hs` ~2674, ~2706). Pair it with an identical positive test that leaves the default
alone — the A/B inside one binary is what proves the criterion is doing the work.
Example: `tests/Arkham/Treachery/Cards/UnawareSpec.hs` (#5429). See [[project_during_your_action_window]].

## "An in-hand card's own modifiers don't exist until a message or two after it lands in hand — `preloadEntities` runs BEFORE each message, `preloadModifiers` AFTER"

The runMessages loop (`Game.hs`) is, per message: `preloadEntities` -> `runMessage msg`
-> `preloadModifiers` (when `shouldPreloadModifiers msg`). So a card put into hand by
message M only becomes an in-hand *entity* on M+1's `preloadEntities`, and its own
`HasModifiersFor` output only lands in `gameModifiers` at the end of M+1.

Worse for a card that is merely **as-if** in hand (Norman Withers' deck top): the
`AsIfInHandFor` modifier that makes `getAsIfInHandEffectCards` see it is itself
preloaded at the end of a message, so the entity appears one message later again.

Symptom on #5544: `addToHand self card >> getModifiedCardCost self.id card` returned the
**printed** cost with no reduction at all — not even the investigator's own
`ReduceCostOf` — which reads exactly like the card being broken rather than the harness
being early.

**How to apply:** in a spec that asserts on an in-hand card's modifiers, put the card in
hand/deck **first**, do the rest of the setup after it (an asset play cascades several
messages), and settle explicitly before asserting:

```haskell
settle :: Investigator -> TestAppT ()
settle self = replicateM_ 2 (gainResources self 0)
```

`gainResources self 0` is a real `TakeResources` message, so the preload wrappers run
regardless of the handler being a no-op. See `JoinTheCaravan1Spec`.
