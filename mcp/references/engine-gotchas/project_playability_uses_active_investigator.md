---
name: project_playability_uses_active_investigator
description: "Card playability is answered relative to gameActiveInvestigatorId, not the iid passed in; getIsPlayableWithResources' now scopes with asActive (#5612)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 265632bc-d986-4bbe-92b8-5286f1f3c536
  modified: 2026-09-05T12:12:28.917Z
---

`getIsPlayable iid …` reads as "can **iid** play this", but the matcher language underneath
answers relative to **`gameActiveInvestigatorId`**, not the argument:

- `Matcher.You` / `NotYou` — `Game.hs:1232` (`view activeInvestigatorIdL`)
- `AssetWithPerformableAbility` — `Game.hs:3452`
- `CardWithPerformableAbility` — `Game.hs:5665`
- `PerformableAbility` — `Game.hs:1883` and `:1990`

So evaluating one seat's hand while a *different* seat is active silently answers about the
wrong investigator, and cards vanish from their owner's own player window. #5612: in a 2-seat
game a window for Sister Mary had left her active; Amanda's `PlayerWindow` was then built with
`active = c07001`, so Knowledge is Power's `cdCriteria`
(`Event/Cards/TheCircleUndone.hs:254`, both `AssetWithPerformableAbility` and
`CardWithPerformableAbility`) asked `getCanPerformAbility c07001` about *Amanda's* Necronomicon
and Scroll of Secrets, `ControlsThis` failed, and the card disappeared from her hand.

**Ability enumeration was already fine** — `getCanPerformAbility` (`Helpers/Ability.hs:73-80`)
scopes `passesCriteria` with `withActiveInvestigator iid` itself. That asymmetry is the
diagnostic tell: the `AbilityLabel`s in the stuck window were correct and only the card labels
were wrong. `handlePlayerWindow` (`Investigator/Runner/Action.hs:436`) wraps `getActions` in
`asIfTurn iid` but left `getPlayableCards iid iid …` unscoped;
`Investigator/Runner.hs`'s `Do (CheckWindows …)` inlined copy scoped neither.

**Fix:** `getIsPlayableWithResources'` (`Helpers/Playable.hs`, the single funnel behind
`getIsPlayable`/`getIsPlayableAfterInitiation`/`filterPlayable`/`getPlayableCards`/
`getOtherPlayersPlayableCards`) now does `active <- getActiveInvestigatorId; if active == iid
then run else asActive iid run`. The guard keeps solo and own-turn windows byte-identical.

**Use `asActive`, not `runCacheReaderT`.** Cache keys are namespaced only by
`gameAllowEmptySpaces` (`Classes/HasGame.hs:37`), never by active investigator, so delegating
the live query cache across an active-investigator swap would let `You`-shaped results collide.
Dropping the cache for the non-active case is the same trade-off `getCanPerformAbility` already
takes. Related: [[project_query_cache_readert_passthrough]].

**Diagnosing this shape:** replay the export with no `--undo` (an empty final queue makes the
engine rebuild the window, so it differs from the saved one), then force the context with
`--answers '[{"tag":"Raw","contents":{"tag":"SetActiveInvestigator","contents":"<other iid>"}}]'`.
If the rebuilt window matches the export's saved question only under the forced active
investigator, the bug is a missing scope, not a card bug. The step `choicePatchDown` entries
record `gameActiveInvestigatorId` changes, so you can read which seat was active per step.
Note `getPlayabilityChecks` (the "why can't I play this" diagnostic path) is still unscoped.

Mirrored at `.claude/references/engine-gotchas/` (see [[project_engine_gotchas_repo_mirror]]).
