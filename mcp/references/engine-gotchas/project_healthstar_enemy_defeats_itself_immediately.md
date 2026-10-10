---
title: project-healthstar-enemy-defeats-itself-immediately
description: "An enemy whose def says healthStar is defeated by the first CheckDefeated at 0 damage — ValueStar evaluates to 0 and the check is damage >= health; the enemy builder must also set healthL .~ Nothing"
---

A `*`-health enemy needs **two** changes, and doing only the first makes the enemy die the
instant anything checks it.

`fromGameValue ValueStar _ = 0` (`GameValue.hs:43`), and the defeat check is

```haskell
-- Enemy/Runner.hs:1858-1860
field EnemyHealth (toId a) >>= traverse_ \modifiedHealth -> do
  when (enemyDamage a >= modifiedHealth) $ do …
```

So `healthStar` on the **def** alone yields `EnemyHealth = Just 0`, and `0 >= 0` defeats the
enemy on the first `CheckDefeated` — before it has taken any damage at all. The `traverse_`
is the escape hatch: when `EnemyHealth` is `Nothing` the whole block is skipped and **no**
defeat check can reach the enemy.

Therefore build the entity with the health cleared, while leaving `healthStar` on the def so
the card browser still prints `*`:

```haskell
eternitysSentinel = enemyWith EternitysSentinel Cards.eternitysSentinel (healthL .~ Nothing)
```

Official precedents: `Enemy/Cards/ThePathToCarcosa/APhantomOfTruth/TheOrganistDrapedInMystery.hs:27`
and `Enemy/Cards/EdgeOfTheEarth/TheHeartOfMadness/TheNamelessMadness.hs:19`.

This is the right shape for the printed wording such enemies carry — "Cannot be damaged.",
"Cannot be defeated." — but note it is independent of the `CannotBeDamaged` /
`CannotBeDefeated` modifiers, which are checked earlier in the same handler (`validDefeat`).
A card that is merely *hard* to defeat wants those modifiers; a card with no health box at all
wants `healthL .~ Nothing`.

Found implementing Eternity's Sentinel (Scourge in the Shadows) in Ages Unwound
(`:ages-unwound:016`). The same trap applies to any `*` stat that feeds a numeric comparison —
check what the comparison does with 0 before relying on the star form.
