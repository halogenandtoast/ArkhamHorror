---
title: project_wrapper_ability_double_accounting
description: "A Free ActionAbility that only nests the REAL ability inside itself is still accounted as a full activate action at a normal index; use the NonActivateAbility index or every activation is counted twice"
---

`ActiveCost.hs` (`PayCostFinished` → `ForAbility`, ~1700-1750) accounts for an ability as a real action based on its **type and index**, not on whether it actually costs an action:

```haskell
isAction = isActionAbility ability && ability.index > 0 && not (isFastAbility ability)
actions  = nub $ [Action.Activate | abilityIsActivate ability] <> ability.actions
```

So an `ActionAbility mempty Nothing Free` "wrapper" ability — one whose whole job is to present a nested choice and then resolve some *other* real ability — gets the full tail pushed for it: `CheckWindows [mkAfter (PerformAction …)]`, `FinishAction`, `TakenActions iid [Activate]`, plus the `ActivateAbility` `#when`/`#after` windows. The nested real ability then pushes all of that **again**, so one activation is counted twice.

Symptoms of the duplicate tail (all seen together in #5298, True Magick (5)):
- reaction cards on `ActivateAbility #after` prompt **twice** for one activation (Sign Magick (3))
- `handleTakenActions` sees the second `[Activate]` after the first entry's `Activate`, opens `PerformedSameTypeOfAction`, and **Haste (2) fires off a single action**
- duplicate `PerformAction #after` window + duplicate `FinishAction` → any "after you perform an action" card (Arm Injury, Time Warp, Dreadful Mechanism, Serpent's Haven) double-fires
- `investigatorActionsPerformed` grows by 2 while `remainingActions` drops by 1 (the tell-tale: a free `[Activate]` entry with no action spent)

**Fix / idiom:** give the wrapper the `NonActivateAbility` index (`Constants.hs`, = 2001). `ActiveCost.hs:1703` short-circuits that index to a bare `UseCardAbility` — no windows, no `FinishAction`, no `TakenActions` — and `Ability.hs`'s `notActivateIndexes` excludes it from `abilityIsActivate`. Precedents: `Investigator/Cards/TonyMorgan.hs` (Bounty additional action), `Asset/Assets/TrueMagickReworkingReality5.hs` (borrowed in-hand spell abilities).

Safe to move off a normal index because:
- the `AbilityIsActionAbility` matcher (`Game.hs` ~1923) is **type**-based and only excludes indices 100–105, so `AssetWithPerformableAbility AbilityIsActionAbility` (Sign Magick (3)'s criterion) still finds the wrapper
- `CannotTakeAction (IsAction Activate)`-style bans still bite, because the *nested* ability is a genuine activate (`Helpers/Ability.hs` ~123 routes `IsAction Activate` through `abilityIsActivate`)
- nothing in `frontend/src` keys off an ability index
- note `abilityActions` still reports `#activate` for the wrapper (`abilityTypeActions` adds it for any non-basic `ActionAbility` regardless of index), so `RepeatableAction` availability checks are unaffected

**How to apply:** any card whose ability is just "choose which other ability to resolve" — and which therefore charges no action cost of its own — belongs at `NonActivateAbility`. If you see `actionsPerformed` gain two entries for one player action, look for a wrapper ability at a normal index. Related: [[project_truemagick_signmagick_ability_exposure]], [[project_activate_dual_action_type]], [[project_test_skip_stale_question]].
