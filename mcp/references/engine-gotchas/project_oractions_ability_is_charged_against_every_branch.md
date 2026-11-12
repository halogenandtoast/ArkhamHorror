---
title: project_oractions_ability_is_charged_against_every_branch
description: "An ability whose designated actions are an OrActions has no declared branch when its cost is paid, so the engine charges the surcharge against the UNION of its branches and records every one as performed"
---

`ActiveCost.activeCostActions` answers `ForAbility a -> a.actions`, and for a
`FastAbility' cost (orActions [#fight, #investigate])` that is **both** branches —
there is nowhere in the `ActiveCost` for a branch the card has not chosen yet.
Everything downstream then treats one activation as performing all of them:

- `getActionCostSurcharges` (`ActiveCost.hs:143`) asks
  `any (matchTarget takenActions performedActions match) ac.actions`, so Frozen in
  Fear's `AdditionalActionCostOf (FirstOneOfPerformed [#move, #fight, #evade]) 1`
  matches on `#fight` and the `ActionCost 0` sentinel appended in `PayForAbility`
  (`Game/Runner.hs`, there so Frozen in Fear can tax a *free* ability like Shortcut (2))
  becomes `ActionCost 1`.
- `startAbilityPayment`'s `FastAbility'` case pushed one `PerformedActions iid [action]`
  **per branch**, so `actionsPerformed` grew two entries — a Fight and an Investigate —
  from one activation, which also feeds `PerformedSameTypeOfAction` and
  `longestUniqueStreak`.

Summoned Hound (1) therefore spent an action on its `[free]` **Fight/Investigate**
*before the fight/investigate prompt was even shown* (#5990). The export tell is
exactly that: at the prompt, `remainingActions` is already down one and
`actionsPerformed` reads `[["Fight"],["Investigate"]]` with `actionsTaken` still `[]`.

**Fix / idiom:** `delayAdditionalCosts` is the flag for "my additional costs wait until
the thing they attach to is known" — added for the sibling bug #4786, where the same
union pulled a location's *additional investigate cost* into the activation. It now
covers the action surcharge too:

- `PayForAbility` omits the `ActionCost 0` sentinel when the ability delays, and
  `getCanAffordAbilityCost` omits it on the same condition — charge and affordability
  must not drift (#5895). Both go through `abilityDelaysAdditionalCosts`
  (`Helpers/Ability.hs`), which is the one place the flag is read.
- `startAbilityPayment` skips the `PerformedActions` pre-record for a delaying ability.
  **Not cosmetic**: the pre-record puts `#fight` into `actionsPerformed` before any
  branch runs, which falsifies `FirstOneOfPerformed` and silently zeroes the surcharge
  the branch is supposed to pay.
- `getAdditionalActionCosts` (`Helpers/Cost.hs`) now runs `AdditionalActionCostOf`
  through the shared `Matcher.matchTarget` instead of spelling out `IsAction`, so it
  sees `FirstOneOfPerformed` and `AnyActionTarget`.
- Each branch charges itself via `CheckAdditionalActionCosts iid target <action> msgs`,
  which passes `abilityEffect a [] cost` — an **empty** action list on purpose, so the
  already-resolved cost is not re-surcharged.

Beware the leftovers:

- A delaying ability no longer gets a blanket affordability check at activation, so
  **each branch must gate itself** (`getCanAffordAdditionalActionCost`) or an
  unaffordable one is offered and `CheckAdditionalActionCosts`' `canPay` guard silently
  drops it after the cost is already exhausted.
- `getAdditionalActionCosts` double-charges a NON-delaying caller (Duke, Local Map,
  Sub-Electron Noise Sensor) for any surcharge the activation sentinel already took.
  Latent today only because no shipped `AdditionalActionCostOf` matcher reaches
  `#investigate` — it was already latent with the `IsAction` spelling.
- A non-delaying `OrActions` ability is still charged and recorded against the union.
  Sword Cane's `[action] Fight/Evade` gets `TakenActions iid [Activate, Fight, Evade]`
  from `PayCostFinished`. Harmless for Frozen in Fear (fight and evade are both on its
  list) but wrong for "different types of actions" logic. The general cure is to narrow
  `activeCostActions` by `activeCostChosenOrAction` for `ForAbility` and give abilities
  the pre-cost ask that `OrActions` **cards** already get (`ActiveCost.hs`, the
  `asksBeforeCosts` / `BeforePlayEvent` pending-event path that Guerrilla Tactics
  answers with `SetActiveCostChosenAction`).

Related: [[project_activate_dual_action_type]], [[project_wrapper_ability_double_accounting]].
