---
title: project_truemagick_signmagick_ability_exposure
description: "Two ability-collection paths (getGameAbilities matcher vs HasAbilities Game action); in-hand asset abilities + True Magick's re-sourced proxies must be surfaced in the matcher path"
---

There are two ability-collection paths and they handled in-hand cards asymmetrically (issue #4905 fix):
- **`getAllAbilities` / `instance HasAbilities Game`** (Game.hs ~6647) — drives `getActions` (the during-turn action list); already concats `gameInHandEntities` abilities unfiltered.
- **`getGameAbilities`** (Game.hs ~1976-2031) — backs the **Matcher DSL** (`AssetAbility`, `AssetWithPerformableAbility`, `select`/`selectMap` on `AbilityMatcher`). It surfaced in-hand **event** abilities (`inHandEventAbilities`) but had **no in-hand asset path** — so matchers couldn't "see" abilities of in-hand assets. Fixed by adding `inHandAssetAbilities` (filtered by `inHandAbility`, i.e. abilities carrying `InYourHand`).

In-hand entities are materialized into `gameInHandEntities` constantly by `preloadEntities` (Game/Runner.hs) for cards with `cdCardInHandEffects = [InHandEffect]`. `getAbilities` is **pure** (no HasGame) so a card cannot enumerate the hand there — game-state-aware re-sourcing must live in the collector, NOT a per-card Metadata cache (drift risk).

**True Magick: Reworking Reality** ("treat True Magick as the revealed Spell") exposes in-hand [Spell] assets' `[action]` abilities re-sourced onto itself via `proxy (CardIdSource c.id) attrs` so `ProxySource.asset == trueMagickId` and `AssetAbility (AssetWithId trueMagick)` matches (`getTrueMagickInHandAbilities` in Helpers/Criteria.hs). It also gains the Spell/Ritual trait at rest **only when a castable in-hand spell exists** (`getTrueMagickGrantedTraits`, applied in its `HasModifiersFor`) — scoped so a resting Tome isn't swept into "all your Spell assets" effects. This is what lets **upgraded Sign Magick (3)** target it (it requires "a different Spell/Ritual asset with a performable [action]"). FAQ confirms True Magick interacts with Sign Magick (3) and Twila Katherine Price (3). `Source.asset` resolves `ProxySource (CardIdSource _) s` to `s.asset`. See [[project_test_useability_empty_windows]].
