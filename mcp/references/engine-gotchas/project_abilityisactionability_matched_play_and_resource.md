---
title: project_abilityisactionability_matched_play_and_resource
description: AbilityIsActionAbility screened out only ability indices 100..105, so the basic PLAY (1000) and RESOURCE (1001) actions matched it — any unconstrained ActivateAbility window fired on them (#5915)
---

Every basic action is modeled as an `Ability` with a reserved index
(`Arkham/Constants.hs`), and the engine opens a real `Window.ActivateAbility`
for two of them:

* the **play** action — `ActiveCost.hs:1996`, `restricted iid PlayAbility … ActionAbility #play`
* the **resource** action — `Investigator/Runner/Action.hs:272`, `restricted iid ResourceAbility … ActionAbility #resource`

`AbilityIsActionAbility` was supposed to screen those out but its filter was a
magic range — `abilityIndex >= 100 && abilityIndex <= 105` — which covers only
fight/evade/engage/investigate/move. `PlayAbility = 1000` and
`ResourceAbility = 1001` fell straight through, so **every** unconstrained
`ActivateAbility … AbilityIsActionAbility` window fired on taking a resource and
on playing a card. Dark Matter's and Ages Unwound's Shambler from the Stars both
did (#5915). Fixed by `basicActionIndex` in `Arkham/Constants.hs`, read at the
TWO sites that must stay in sync: `abilityMatches` (`Game.hs:2065`) and the bulk
`select` twin (`Game.hs:2175`).

Two things not to "fix" along with it:

* **Resign (99) is a real [action] ability.** The RR glossary says resign
  abilities carry a Resign *designator* and are initiated with the Activate
  action, so they belong in the matcher.
* **The draw action never opens an `ActivateAbility` window at all** — only the
  three sites above do (`ActiveCost.hs:1971/1996`, `Runner/Action.hs:273`), so it
  was never involved.

The engine already had the right predicate in `abilityIsActivate`
(`Ability.hs:92`), which excludes `PlayAbility`/`ResourceAbility`/`NonActivateAbility`
and anything `basic`. `AbilityIsActionAbility` just never got the same treatment,
and the index range hid it.

When a window matcher needs "a printed [action] ability",
`AbilityIsActionAbility` is now correct — but it is only safe *unconstrained*
because of `basicActionIndex`. Nearly every other call site is constrained to a
card source (`AbilityOnAsset`, `AbilityOnLocation`, `AbilityOnEnemy`,
`AbilityOnEncounterCard`), which is why the bug stayed invisible for so long:
the investigator-sourced play/resource abilities fail those anyway. If you add a
new reserved ability index for a basic action, add it to `basicActionIndex` too.

Related: [[project_abilitylabel_without_windows_rejects_everything]].
