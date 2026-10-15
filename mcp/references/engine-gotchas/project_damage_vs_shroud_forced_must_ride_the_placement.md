---
title: project_damage_vs_shroud_forced_must_ride_the_placement
description: A "damage equals its shroud value" Forced on AnyWindow collapses the room mid-investigation, because LocationJustShroud is the MODIFIED shroud and a Flashlight dips it
---

"**Forced** – If there is damage on <room> equal to its shroud value: Flip
<room>" (The Fall of the House of Usher's eight [[Decadent]] rooms) reads like a
*state* condition, and the engine's idiom for a state-triggered Forced is
`forced AnyWindow` with a self-falsifying criterion — Café Luna (Bastion of
Remembrance), Rainy London Streets, Falcon Point Approach, A Dream Betwixt and
Barrier Core all ship that way. It is the wrong shape here, and the reason is
the shroud side of the comparison, not the Forced side.

The criterion has to read the shroud through a calculation, and both available
fields give the **modified** value: `LocationJustShroud` is
`getModifiedShroudValueFor` (`Game.hs:4876`), and `LocationShroud` is the same
behind a `Maybe`. Shroud reduction is extremely common on the player side —
Flashlight, Lantern, "Look what I found!", Dr. Milan-style investigation
riders — and those modifiers are live for the duration of the investigation.
So a room at damage 1 / printed shroud 3 reads `1 >= 1` for as long as a
Flashlight is applied, and an `AnyWindow` Forced fires inside that window and
collapses a room that took no new damage.

`onlyOnce` does not save it: the collapse is legal exactly once per room
anyway, so the guard happily spends the one use on the false positive.

Hang it on the placement instead:

```haskell
collapseForced tip a n =
  withI18nTooltip tip
    $ onlyOnce
    $ restricted a n damageAtShroud
    $ forced (PlacedToken #after AnySource (TargetIs $ toTarget a.id) #damage)
```

`Location/Runner.hs:378` raises `Window.PlacedDamage` (a bidirectional synonym
for `PlacedToken source target Damage n`) and applies the count in the same
handler, so at `#after` the damage is already on the card. The criterion is
then only ever evaluated at a moment when damage actually arrived, which is the
only way the comparison can newly become true for real.

What this costs: a *permanent* shroud decrease onto existing damage will not
trigger the flip. Nothing in that box does it, and the trade is strongly worth
it — a missed collapse is recoverable by placing one more damage, a spurious one
destroys a room the players were still using.

The related trap in the other direction is
[[project_always_true_forced_on_anywindow_makes_a_card_unplayable]]: there the
hazard is a criterion that NEVER falsifies. Here the criterion does falsify
(the room becomes [[Ruined]] and its `b` face prints no such ability) — the
hazard is that it becomes true when nothing happened.
