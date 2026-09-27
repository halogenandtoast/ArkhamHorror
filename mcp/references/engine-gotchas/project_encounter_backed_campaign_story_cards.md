---
name: project_encounter_backed_campaign_story_cards
description: "Earned campaign story cards with encounter backs are dropped by deck reload; only permanents survive, and SetupInvestigator now puts them into play from campaignStoryCards"
metadata: 
  node_type: memory
  type: project
  originSessionId: 5fb0ba49-333b-4654-8f1f-1bf9e98d9dde
  modified: 2026-08-13T10:59:38.983Z
---

`AddCampaignCardToDeck` stores the card in `campaignStoryCards` whichever side it is
printed on — `setOwner` (`Arkham/Card.hs`) sets `ecOwner` for encounter cards just as it
sets `pcOwner` for player cards. So ownership checks like Dark Matter's Interlude I
"do you have Heir to Carcosa" (`getCampaignStoryCards`) work fine for encounter-backed
cards.

What does **not** work is the deck: both reload paths keep only player cards —
`Campaign/Runner.hs` `LoadDeck … <> mapMaybe (preview _PlayerCard) storyCards` and
`Helpers/Campaign.hs` `getCurrentDeck`. An encounter-backed earned card is therefore gone
from the deck in every later scenario.

**How to apply:** an encounter-backed card can only be earned if it is `permanent`.
`SetupInvestigator` (`Investigator/Runner.hs`) puts those into play in the same step that
plays the deck's own permanents, filtering `campaignStoryCards` for the investigator by
`isJust . preview _EncounterCard`, `cdPermanent`, and not `CannotPutIntoPlay`. Do **not**
convert such a card to a player-card def with a `customBack` to get it into the deck —
that pulls it out of the encounter pool and it stops being gathered (Dark Matter's Heir to
Carcosa vanished from the scanning deck that way; `addScanningDeck` filters
`amongGathered AnyCard`).

Only Dark Matter's Heir to Carcosa uses this today. The two other encounter-backed
permanents in the codebase (`creepingPoison` 04101, `wrathOfYig` 53080) are ordinary
encounter treacheries that never enter `campaignStoryCards`.

Related: [[project_upgradedeck_replaces_campaign_deck]],
[[project_scan_icon_backs_use_other_side]], [[project_tfa_poison_storycards]]
