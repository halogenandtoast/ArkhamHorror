---
title: project_tfa_poison_storycards
description: "TFA Poisoned weakness persists in campaignStoryCards, not campaignDecks or threat area; interlude poison checks must read storyCards"
---

The Forgotten Age "Poisoned" weakness (card `04102`) is a **permanent player-card weakness** (`cdPermanent = True`). `becomePoisoned` calls `addCampaignCardToDeck`, whose handler (`Campaign/Runner.hs` `AddCampaignCardToDeck`) stores the card in **`campaignStoryCards`**, NOT `campaignDecks`. `campaignDecks` only ever holds the ArkhamDB decklist (set by InitDeck/UpgradeDeck) and is never augmented with the permanent weakness.

Consequences when checking "is this investigator poisoned":
- During a **scenario**: `getIsPoisoned` (threat-area treachery select) is correct — the permanent is put into the threat area at setup.
- During a **campaign interlude** (e.g. ResupplyPoint / St. Mary's): there are zero treachery entities in play, so `getIsPoisoned` returns False. Must check `attrs.storyCards` instead.
- `getPoisonedInvestigators` (TheForgottenAge.hs) originally checked `attrs.decks` and so could never find Poisoned — fixed to also scan `attrs.storyCards`.

**Why:** Issue #4857 — St. Mary's offered no "remove Poisoned for 3 XP" option because the interlude poison check looked in the wrong zone.
**How to apply:** For any TFA interlude poison logic, detect poison via `getPoisonedInvestigators attrs` (storyCards-aware), not `getIsPoisoned`. `removeCampaignCardFromDeck` correctly strips from both storyCards and decks.
