---
name: project_resetgame_wipes_assets_but_not_slots
description: "ResetGame deletes non-Persist assets/events from gameEntities but investigatorSlots lives on InvestigatorAttrs; skipInvestigatorSetup skips the slot rebuild and leaves dangling AssetIds"
metadata:
  type: project
---

`ForTarget GameTarget ResetGame` (`Game/Runner.hs` ~599) deletes every non-`Persist` asset
and every event from `gameEntities`, but `investigatorSlots` is a field on
`InvestigatorAttrs` — **not** part of `gameEntities` — so it survives the wipe with dangling
`AssetId`s. What normally cleans that up is the *next* message:
`CampaignStep (ScenarioStepWithOptions …)` (`Campaign/Runner.hs` ~90) pushes
`ForInvestigators [] ResetGame`, whose handler (`Investigator/Runner.hs` ~556) rebuilds
`investigatorSlots = defaultSlots a.id`.

That message is gated on `not opts.skipInvestigatorSetup`. Hemlock Vale's `afterPrelude`
(`Campaigns/TheFeastOfHemlockVale/Helpers.hs` ~298) is the only caller that sets
`scenarioOptionsSkipInvestigatorSetup = True` — all 15 prelude → survey transitions
(Day 1/2/3 × 5 scenarios). Any asset still in play there and not marked `Persist` by
`makePreparationsForNextSurvey` left a dead id in a slot for the whole next scenario.

**Why:** the crash is far from the cause. Reading a slot occupant means
`field AssetSlots` / `field AssetCardId`, which throws `MissingEntity "Unknown asset"`.
#5593 surfaced as "assigning Dr. Rosa Marquez to Bob Jenkins crashes" — Bob's Accessory
slot held a dead Moonstone, and `TakeControlOfAsset` → `InvestigatorPlayAsset` →
`InvestigatorClearUnusedAssetSlots` walks every occupant. Bob Jenkins was incidental (his
`_ ->` catch-all is just the re-raising frame); the other two investigators only worked
because their slot occupants happened to be the `Persist`-marked ones.

**How to apply:** fixed at the wipe itself — ResetGame now prunes each investigator's slots
to the surviving assets (`retainSlotAssets` in `Helpers/Slot.hs`) and drops slots whose
`slotSource` is a wiped asset or any event. `InvestigatorClearUnusedAssetSlots` also
partitions occupants with `fieldMay AssetCardId` first so a dangling id can never hard-crash
a saved game again. Spec: `tests/Arkham/Game/ResetGameSpec.hs`.

Secondary, still open: ResetGame destroys the *player card* of every wiped asset too. Fine
when the deck is rebuilt; with `skipInvestigatorSetup` the deck persists, so the card is
gone from the campaign for good (Bob permanently lost his Moonstone). See
[[project_player_back_story_asset_pool]] for related card-zone bookkeeping.
