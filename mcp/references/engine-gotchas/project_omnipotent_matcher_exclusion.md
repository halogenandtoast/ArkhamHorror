---
title: project_omnipotent_matcher_exclusion
description: Omnipotent enemies (Dagon/Hydra Deep in Slumber) are excluded from all default enemy matchers; use IncludeOmnipotent
---

Enemies with the `Omnipotent` modifier (Into the Maelstrom's Dagon/Hydra "Deep in Slumber" forms; the Awakened forms are NOT Omnipotent) are invisible to every normal `EnemyMatcher`: `Arkham/Game.hs` (~3595) silently appends `EnemyWithoutModifier Omnipotent` to the default matcher branch. Only `IncludeOmnipotent matcher` (Game.hs ~3580) sees them.

**How to apply:** Any card that references a slumbering ancient one via `enemyIs`/`select`/`exists` must wrap the matcher in `IncludeOmnipotent` (the established pattern — Dagon's own abilities self-reference with `thisExists a (IncludeOmnipotent ReadyEnemy)`). Plain `enemyIs Cards.dagonDeepInSlumberIntoTheMaelstrom` returns nothing even with Dagon in play.

**Why:** issue #4964 — Dagon's Brood / Hydra's Brood forced ("on engage, place doom on the ancient one") never fired because their restriction `exists $ mapOneOf enemyIs [dagon…]` couldn't see the Omnipotent slumbering Dagon → restriction False → forced never offered. Fixed by wrapping in `IncludeOmnipotent`.

Test-harness note: an enemy's on-engage forced is a prompt — resolve with `useForcedAbility` (see SilverTwilightAcolyteSpec). Read an Omnipotent enemy's doom via the direct field accessor `dagon.id.doom`, never via a matcher.
