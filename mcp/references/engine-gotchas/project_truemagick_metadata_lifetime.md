---
title: project_truemagick_metadata_lifetime
description: "True Magick (5) stays masquerading as the borrowed spell through the whole ActivateAbility #after window (Metadata clears only on ResolvedAbility, delivered via MoveWithSkillTest), so it can't be revealed as a second in-hand spell inside that window"
---

Open finding (investigated for #5298, **not fixed** — the #5298 fix was the wrapper-index one, [[project_wrapper_ability_double_accounting]]).

`Asset/Assets/TrueMagickReworkingReality5.hs` holds `Metadata { currentAsset :: Maybe Asset }` while it borrows an in-hand [Spell]. The slot does three jobs: copy the borrowed card's traits (`HasModifiersFor`), expose the borrowed card's abilities *instead of* the wrapper (`getAbilities`), and route every message to the throwaway inner asset entity (`RunMessage` catch-all).

It is cleared on `ResolvedAbility ab` whose `ab.source` is a `ProxySource` onto True Magick. That message is pushed as `MoveWithSkillTest (ResolvedAbility ability)` at `Investigator/Runner/Action.hs` ~643, i.e. **behind** the whole `ActiveCost` tail. Queue order for a borrowed activation (verified in the #5298 export, step 367):

```
CheckWindows[After PerformAction]  FinishAction  TakenActions  CheckWindows[After ActivateAbility]  MoveWithSkillTest(ResolvedAbility)
```

So for the entire `ActivateAbility #after` window — the window where Sign Magick (3) and every other "after you activate an ability" reaction resolves — True Magick still reads as the borrowed spell and `getAbilities` returns the borrowed card's abilities, not the wrapper. User-visible: inside that window you cannot reveal a *different* in-hand spell (the reporter's "can't use True Magick as a Blood Pact"). By the card text that is legal — the borrowed ability has fully resolved, and Blood Pact's `[fast]` abilities cost doom, not True Magick's (already spent) charge.

Rejected approaches:
- **Clear on the borrowed ability's `FinishAction`** (it sits before the after-window). `FinishAction` carries no ability/source reference, and nested actions *do* occur while the slot is set (Haste's granted action, an AoO, Sign Magick's zero-cost activation), so it would clear early. Worse, `FinishAction` is only pushed when `isAction` — a borrowed `[fast]`/reaction ability would never clear at all.
- **Reorder `ResolvedAbility` before `afterActivateAbilityWindow` in ActiveCost.** Not reachable from the card, and `ResolvedAbility`'s queue position is load-bearing for every ability in the game (`resolveAbilityAndThen`, `Effect/Runner.hs` ~126 `isEndOfWindow (EffectAbilityWindow ab.ref)`, `Message/Lifted.hs` ~2314-2392).

Recommended (own change, own tests): make `Metadata` a **stack** of borrowed assets — push on the `ProxySource` `UseCardAbility`, pop on the matching `ResolvedAbility`, route messages/traits to the head, and have `getAbilities` return `getAbilities head <> wrapperAbilities`. The single slot is precisely why the wrapper is hidden today (re-entering would clobber the in-flight borrowed entity); a stack removes that reason. Two things it must handle:
- **Persistence back-compat.** `With`'s `ToJSON` merges the metadata object into the asset attrs object (`Prelude.hs` ~291), so `currentAsset` is a top-level key in every saved game with True Magick in play. Reshaping it needs a tolerant `FromJSON` (absent/null → `[]`, object → `[x]`, array → as-is).
- **Message routing while two are stacked.** The catch-all forwards to the head only; a lasting effect of the *outer* borrowed ability that still consults its inner entity would break. In practice the outer effect has finished by the time the after-window opens (that is the whole point), but it needs a test.

**How to apply:** don't reach for the borrowed-asset slot as a general "True Magick is currently X" signal — it stays set through the after-windows and clears late. Related: [[project_truemagick_signmagick_ability_exposure]].
