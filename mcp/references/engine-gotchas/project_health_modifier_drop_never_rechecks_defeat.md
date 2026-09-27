---
name: project_health_modifier_drop_never_rechecks_defeat
description: "Defeat is only rechecked when damage is assigned, never when an enemy's modified health falls — every clue/stack-scaled health card needs its own CheckDefeated intercept"
metadata: 
  node_type: memory
  type: project
  originSessionId: eb72cb04-e5b4-4ba7-8ac3-f96e1f768764
  modified: 2026-09-22T04:46:29.077Z
---

`CheckDefeated` is pushed only from damage assignment (`Enemy/Runner.hs`). Nothing recomputes
defeat when a `HealthModifier` makes an enemy's modified health *drop to or below* the damage
already on it, so the enemy sits in play overkilled. Three instances so far:

- Randall Tillinghast — `+1[per_investigator]` health per card under Tillinghast Esoterica;
  fixed in `539c50cbe6` by `selectEach (enemyIs ...) $ checkDefeated` in
  `Act/.../TheDoomOfArkham/ThePhantomShop.hs` after the shop's action removes a card (#5729).
- Nyarlathotep (True Shape) — act 5 `06294` gives `-1` health per clue the investigators hold;
  he survived at 14 health / 6 damage with 8 clues out (#5749).
- Anette Mason (Reincarnated Evil) — `-2` health per clue, same as her counterpart
  Carl Sanford (Deathless Fanatic), who alone had the guard. Fixed alongside #5749.

**Why:** the modifier is applied correctly (it shows in `gameModifiers` as
`HealthModifier -n`); only the *check* is missing. So a debug export looks entirely healthy —
`enemyHealth` is still the printed `Fixed` value and `defeated` is `false`.

**How to apply:** when a card's health scales off a changing quantity, add an intercept on the
message that changes that quantity. For clue-scaled health the hook is `After (GainClues {})`
— pushed by **both** clue paths in `Investigator/Runner.hs` (the plain `GainClues` handler and
the `Do (DiscoverClues ...)` handler), and it fires after the tokens land, so the recomputed
modifier is already correct:

```haskell
After (GainClues {}) -> do
  checkDefeated GameSource attrs   -- lifted; or `push $ checkDefeated GameSource attrs`
  pure e
```

The `CheckDefeated` handler already guards `not enemyDefeated`, `CannotBeDefeated`,
`CanOnlyBeDefeatedBy` and swarm, and opens the normal `EnemyWouldBeDefeated` windows, so the
intercept needs no conditions of its own. Related: [[project_arkham_replay_tool]].
