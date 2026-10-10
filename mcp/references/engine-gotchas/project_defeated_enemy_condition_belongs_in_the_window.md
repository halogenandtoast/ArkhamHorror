---
title: project-defeated-enemy-condition-belongs-in-the-window
description: "A Forced-on-own-defeat ability must put \"this enemy\" inside the EnemyDefeated window matcher, never in an ability criterion — at #after the enemy is out of play and a criterion selectAny returns nothing"
---

An enemy ability that triggers on its **own** defeat must express "this enemy" (and any
qualifier like `IsHost`) **inside the `EnemyDefeated` window matcher**, not as an ability
criterion. `restricted a 1 (exists $ be a <> IsHost) $ forced $ EnemyDefeated #after …`
compiles clean and silently never fires.

By the time the `#after` window resolves, the enemy is gone from play and lives only in
`ScenarioDefeatedEnemies`, so a criterion's `selectAny`/`exists` over in-play enemies returns
nothing and `getCanPerformAbility` rejects the ability. The defeat-time attrs are re-inserted
into the game env **only** by `getEnemiesMatching`'s `DefeatedEnemy` arm (`Game.hs:3884-3896`,
left-biased so it never shadows a live entity), and the only thing that reaches that arm is the
window matcher itself: `Helpers/Window.hs:1989` widens an `EnemyDefeated`/`EnemyEngaged`-style
enemy submatcher to `orM [matches enemyId (DefeatedEnemy m), matches enemyId m]` **when and only
when `timing == #after`**. An ability criterion is evaluated outside that widening.

So the correct shape is:

```haskell
-- WRONG: criterion is evaluated against in-play enemies, which no longer include `a`
extend1 a $ restricted a 1 (exists $ be a <> IsHost) $ forced $ EnemyDefeated #after Anyone ByAny AnyEnemy

-- RIGHT: the condition rides the window, which re-reads the defeat-time attrs
extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after Anyone ByAny (be a <> IsHost)
```

Putting it in the window is also what makes swarm-aware conditions correct: the re-inserted
attrs still carry their `AsSwarm` placement, so `be a <> IsHost` fires for the host's defeat and
**not** for a swarm card's, which a criterion could not distinguish at all. Found implementing
The Myriad Gentleman (One of Many) in Ages Unwound (`:ages-unwound:220`), whose Forced reads
"After the host Myriad Gentleman is defeated".

The same reasoning applies to any `#after` window naming an entity that the window's own event
removed from play — prefer the window's matcher over a criterion whenever the trigger *is* the
removal. See [[project_after_dealt_damage_is_post_defeat]] for the damage-side analogue, and
[[project_swarm_moves_as_one_unit]] for why host-vs-swarm matters.
