---
name: project_fight_reach_needs_availability_and_selection_halves
description: A card that widens where a Fight can reach needs BOTH an AbilityTarget modifier (so the fight ability is offered) and an investigator-targeted one (so selection honours it); the effect alone is a no-op, and the same permission also decides whether its window may be skipped
metadata:
  node_type: memory
  type: project
  originSessionId: 0b248932-ceea-4ae4-8e2f-b081a659fa09
  modified: 2026-10-17T07:35:18.745Z
---

Widening a Fight's reach takes **two** modifiers in two different places, and
shipping only the second silently does nothing:

1. **Availability** — `canDoAction'` (`Helpers/Ability.hs`, `Action.Fight`,
   non-enemy/non-location source) reads `EnemyFightActionCriteria` off
   `getModifiers (AbilityTarget iid ab.ref)`. Without one it falls back to
   `CanFightEnemy`, i.e. same location only, and the asset's fight ability is
   **never offered**. This lives in the *card's* `HasModifiersFor` and should be
   `CanModify`-wrapped.
2. **Selection** — `ChooseFightEnemy` (`Investigator/Runner.hs`) reads
   `getModifiers investigator` under `withoutCanModifiers`, so it needs a
   **bare** override, normally created by the card's effect in `UseThisAbility`.

Marksmanship (1) and Telescopic Sight (3) are the reference implementations;
both carry both halves.

**Why:** this bites hardest on a `[reaction] When you activate a Fight ability
…` card, because the trigger window only opens if the fight ability was
offered. Ages Unwound's Landing ("this attack targets an enemy in the Entrance
Hall") had only half 2, so with no enemy at Landing Shrivelling's fight button
never appeared and the reaction could not fire — a chicken-and-egg that reads
in-game as "the card does nothing".

**How to apply:** an override *replaces* the standard criterion rather than
widening it, so half 1 must include the investigator's own location
(`be a` / `withinDistance`) or ordinary same-location fights disappear. Keep
half 1 `CanModify` so it stays invisible at selection and cannot collide with
half 2's bare override (`>1` bare override is `error "multiple overrides
found"`). See [[project_fight_overrides_and_canmodify_scope]] for how the two
readers combine (OR for availability, AND at selection).

## The skip button is the third half

A provisional permission also decides whether its window may be skipped. The
`[reaction] When you activate a Fight ability …` shape opens a window on the
activation, and skipping it strands the action it was going to retarget.

`getAllAbilitiesSkippable` (`Investigator/Runner.hs`) takes `pending` — the
sources offering something in the window — and its `ActivateAbility` branch
requires the activated ability to still pass `meetsActionRestrictions` with
those sources' modifiers removed (`withoutModifiersOfAny`, `GameEnv.hs`, the
list form of the existing `withoutModifiersOf`). No valid target without them
means no skip button; a second enemy in ordinary reach brings it back.

**Do not reach for `withoutCanModifiers` here.** It drops every `CanModify` in
the game, and Longbow (3), Becky (2), British Bull Dog (2) and Enchanted Bow (2)
all spell "ignore aloof" that way while resolving it on the `ChooseFight` they
push themselves (`chooseFightEnemyMatchEdit` / `mkChooseFightMatch`). Their
permission needs nothing from the window, so stripping it wrongly pins their
windows open. Scoping to `pending` is what separates "needs a reaction to
complete it" from "granted outright". Landing and Telescopic Sight (3) are the
former; they supply the override AND the reaction.

Marksmanship (1) is the same shape from hand, but its modifiers hang off the
in-hand entity (id via `unsafeCardIdToUUID`) rather than the `Card`, so
`playableCards` is deliberately NOT folded into `pending` yet.
