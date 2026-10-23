---
title: project_choosen_zero_amount_never_terminates
description: A ChooseN with a non-positive amount never ends on count (the check was `n - 1 == 0`), so a 0-resource cost charged Preston 4 via Family Inheritance's AsIfResourcePool prompt
---

`Entity/Answer.hs`'s `ChooseN` answer handler ended the question only when `n - 1 == 0`, so
an amount of 0 counted **down** forever: 0, −1, −2, … Nothing else stops it except the
choice list emptying, or the sibling "every remaining choice is identical" shortcut
(`all (== m1) mrest`, whose `take (n - 1)` is `[]` for a non-positive `n`, silently dropping
the follow-up `Ask`).

`handleSpendResources` (`Investigator/Runner/Action.hs`) is how a player met one. When the
investigator has an `AsIfResourcePool` asset with resources on it — **only Family
Inheritance**, so only Preston Fairmont — payment goes down a "which pool do you want to
spend from?" branch:

```haskell
  resourcePools <- forToSnd [aid | AsIfResourcePool aid <- mods] (field AssetResources)
  ...
      push $ chooseOrRunN player n
        $ concatMap (\(aid, k) -> replicate k (targetLabel aid [RemoveResources ...])) resourcePools
        <> replicate a.resources (ResourceLabel iid [Do (SpendResources iid 1)])
```

with no check on `n`. So Lola Santiago (3) at a location whose shroud a Flashlight had
reduced to 0 cost `SpendResources iid 0` → `ChooseN 0` over 11 options. The player clicked
4 times before the homogeneity shortcut released them, losing all 4 resources off Family
Inheritance — while the cost itself recorded `ResourcePayment 0` (#5900).

It was reported as "Double or Nothing doubled the shroud". It hadn't: `DoubleDifficulty` is
read only by `getModifiedSkillTestDifficulty` and never touches shroud, Flashlight's
`ShroudModifier (-2)` *was* folded into `field LocationShroud` (it is still live in the fast
window between the test resolving and `EndSkillTestWindow_`), and the `4` was a coincidence.
The lesson for triage: a cost the engine computed *correctly as 0* can still take money.

Fixed by returning early on `n <= 0` in `handleSpendResources` (no prompt and no
`Window.SpendsResources`, since spending nothing is not spending), skipping
`Ask (ChooseN n _)` for `n <= 0` in `Game.hs` beside the existing empty-list guard, and
checking `n - 1 <= 0` in the answer handler.

When adding a `chooseOrRunN`/`chooseN` whose amount is computed, note that `chooseNM` guards
`n > 0` but `chooseOrRunNM` does not. Spec:
`tests/Arkham/Asset/Assets/LolaSantiago3Spec.hs`.

Related: [[project_chaos_bag_removal_window_and_choosen_cost_fragmentation]] (the other
`ChooseN` cost trap: one message per pick, so a window opened by the payment fires n times).
