---
title: project_campaign_modifiers_for_all
description: "Campaign-wide, campaign-agnostic \"for the remainder of the campaign\" modifiers via campaignModifiersForAll"
---

For modifiers that must apply to ALL investigators "for the remainder of the campaign" — and that must survive even when a scenario is played inside a *different* campaign than its home mini-campaign — use the `campaignModifiersForAll :: [ModifierType]` field on `CampaignAttrs` (Arkham/Campaign/Types.hs).

**Why this and not a campaign-level `HasModifiersFor`:** a per-campaign `HasModifiersFor` instance (e.g. on `GuardiansOfTheAbyss`) only fires when that campaign is active. Standalone/side scenarios played in a full campaign run under a different campaign type, so that instance never runs.

**How it works:**
- Add: `push $ AddCampaignModifiersForAll [<ModifierType>]` — handled generically in `defaultCampaignRunner` (nub-appends to the field), so it works for any campaign.
- Remove: `push $ RemoveCampaignModifiersForAll [...]` (filter notElem) — keep the stored modifier in sync if the imposing effect is later lifted (e.g. crossed-out campaign-log notes).
- Emit: `Game.preloadModifiers` (Arkham/Game.hs) expands the field onto every investigator via `select Anyone`, marking them `setActiveDuringSetup` so they apply during setup too, then `tell $ MonoidalMap.singleton (toTarget iid) mods`. Can't live in the `CampaignAttrs` `HasModifiersFor` instance — `select`/Query instance isn't in Campaign.Types' transitive imports; would also create import cycles.
- Persistence is free: `CampaignAttrs` is serialized; FromJSON defaults `modifiersForAll` to `mempty` for old saves.

`CannotPutIntoPlay` is honored at `SetupInvestigator` (Investigator/Runner.hs): cards that would start in play (signature `investigatorStartsWith` like Duke, and `cdPermanent` cards) are left in the deck if `cardMatch`ed by a `CannotPutIntoPlay` modifier. Requires the modifier to be setup-active (above), since `gameInSetup=True` until `EndSetup` and the modifier cache is filtered to `modifierActiveDuringSetup` during setup.

First user: Guardians of the Abyss "taken by the abyss" — unique Ally taken => `[CannotPlay (cardIs card), CannotPutIntoPlay (cardIs card)]` campaign-wide; lifted by `crossOutTakenByTheAbyss` (Resolution 1, curse of slumber lifted). See [[project_drowned_city]] for the related Abyss content area.
