---
name: project_asset_controlled_by_saw_hand_cards
description: "`AssetControlledBy` matched the pseudo-asset entity of a card still in HAND, so \"an asset you control\" saw a Firearm Robert Castaigne (4) had merely revealed (#5695)"
metadata:
  type: project
---

A card sitting in hand can get a real `Asset` entity, with `assetPlacement = StillInHand iid`
**and `assetController = Just iid`**. Two producers:

- `cdCardInHandEffects` cards, preloaded by `preloadHandEntities` (`Game/Runner.hs`) — Foundation
  Intel, Pelt Shipment, Clasp of Black Onyx, The Devil XV, The Tower XVI, Occult Scraps, G-Men.
- `withCardEntity @AssetId` (`Message/Lifted.hs`) → `AddCardEntity` (`Game/Runner.hs`), a
  temporary entity for "reveal a card from your hand and use it" — both Robert Castaigne
  printings, Ad Hoc, Pushed to the Limit, Knowledge Is Power.

The controller has to be set so the card's own `ControlsThis`-gated abilities can be performed
(every Firearm's fight ability is `fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis`). But
`getAssetsMatching'`'s `AssetControlledBy` branch only ever read the controller field — it never
looked at placement — so **every** "an asset you control" select saw the hand card.

Michael McGlen used Robert Castaigne (4) to attack with a Sawed-Off Shotgun from hand. During
the resulting skill test's fast window, Custom Modifications (Leather Grip → `BecomesFast`)
passed its criteria (`exists $ AssetControlledBy You <> #firearm <> …`) even though McGlen
controlled no Firearm in play, and the attach prompt offered a `TargetLabel (AssetTarget …)`
for an asset with no board presence — nothing to click, game soft-locked.

Fixed in two paired places:

- `Game.hs` `AssetControlledBy` now returns `False` for `Placement.StillInHand _`.
- `Helpers/Criteria.hs` `Criteria.ControlsThis` gained a fallback
  `select (AssetWithPlacement $ StillInHand iid)`, since it was the one consumer that needed
  the old behaviour.

**Why:** the pseudo-entity exists so modifiers and abilities resolve, not to put the card in
play. Control is a play-state concept; `AssetOwnedBy`/`OwnsThis` were left alone because you
*do* own a card in your hand.

**How to apply:** `StillInHand` is the only out-of-play placement assets actually use
(`HiddenInHand` is enemies/treacheries only, `StillInDiscard` only a Skill — Persistence (1)),
so it is the single placement worth excluding. When adding a matcher that means "in play",
remember `getAssetsMatching'` filters only `placement.outOfGame` — `InPlayAsset` is the explicit
opt-in wrapper and out-of-play entities are otherwise included by default. The whole upgrade
family (`getUpgradeTargets` in `Message/Lifted/Upgrade.hs` — Iron Sights, Extended Barrel (1),
Custom Grip, Custom Ammunition (3), Fine Tuning (1), Tinker, Jury-Rig, Trusted, Ad Hoc, …)
shares the same `AssetControlledBy You` criteria and was wrong the same way.
See [[project_cards_beneath_asset_have_no_entity]] and
[[project_in_discard_ability_outlives_the_discard]].
