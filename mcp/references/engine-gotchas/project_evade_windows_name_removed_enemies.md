# Evade windows deliberately still name a removed enemy

`enemyMatches` (`Helpers/Window/Enemy.hs:171`) is
`orM [matches eid m, matches eid (OutOfPlayEnemy RemovedZone m)]`, and `Helpers/Window.hs` uses it
for `EnemyEvaded`, `EnemyWouldBeEvaded`, `SuccessfulEvadeEnemy` and friends. That is on purpose:
an evasion can discard the enemy on its way through (Kymani), and "after you evade" reactions must
still fire. Evade windows are explicitly **not** tombstone subjects
([[project_leave_play_tombstones]]), this OR is their read-back mechanism.

The trap: the enemy may already be **fully dead** when the window resolves — not discarded *by* the
evade, but defeated earlier in the same skill test. Damage resolves before the event's evasion, so
`Defeated → Discard → RemovedFromPlay → RemoveEnemy` has already run and the entity is sitting at
`OutOfPlay RemovedZone` with `defeated = True`, `exhausted = True`, waiting for
`clearRemovedEntities`.

## Two rules

**A card that reads an enemy id out of an evade window must gate its window matcher with
`InPlayEnemy`.** `InPlayEnemy` beats both arms: the plain arm never sees `OutOfPlay` enemies
(`restrictToInPlayZones`, `Game.hs:3753`), and the `OutOfPlayEnemy RemovedZone` arm is filtered
back down to `isInPlayPlacement` (`Game.hs:3921`), which `OutOfPlay RemovedZone` is not. The
`EnemyDefeated` window matcher already does this for its `#when` timing
(`Helpers/Window.hs:1863/1873/1885`). Gating the matcher, not the `RunMessage` body, also stops the
ability being *offered*.

Note `Game.hs:3744` calls `InPlayEnemy` "redundant (a no-op)" — that is true only of the ordinary
query path. Inside `enemyMatches`' second arm it is load-bearing. Do not delete it.

**`PlaceEnemy` refuses to return a defeated enemy to play.** `Enemy/Runner.hs` guards the handler
with `enemyDefeated && isInPlayPlacement placement && not (isInPlayPlacement a.placement)`. Only
the `AtLocation` branch routed an out-of-play enemy through `EnemySpawn` (which does
`defeatedL .~ False`); `InTheShadows` and every other placement called `handlePlacement` directly,
so the enemy came back still flagged defeated — and
`CheckDefeated … | not enemyDefeated` plus `ReadyExhausted | not enemyDefeated` then no-op forever.
The `not (isInPlayPlacement a.placement)` half keeps `EnemyDefeated #when/#after` reactions
working, since the enemy is still on the table there
([[project_ifenemydefeated_resolves_after_disposal]]).

Deliberate resurrections (`HuntingHorror`, `TheAmalgam`, `LaComtesseSubverterOfPlans`,
`TheConductorBeastFromBeyondTheGate`, `insteadOfDiscarding`) clear `defeatedL` before returning the
enemy, so the guard never sees them.

Bug this explains: #5610 — an Acolyte killed by chapter-2 Daniela's elder sign during the parley
test that then evaded it. *Agents of the Dark* (`09567`) placed the corpse back in the shadows,
where `Concealed/Runner.hs`'s `DoStep 1 (Flip …)` (`EnemyWithPlacement InTheShadows <>
EnemyWithTitle …`) later exposed it onto a location as an undying, permanently exhausted enemy.

Related: [[project_leave_play_tombstones]], [[project_ifenemydefeated_resolves_after_disposal]],
[[project_placeenemy_inplay_skips_engagement]].
