---
title: Blood Money's setup gathers and sets aside the Infected encounter set
date_added: 2026-10-15
source: Alex Werner, FFG Game Rules Specialist (rules question response; stated as a likely official erratum)
affects:
  - The Blood Blight (13118)
  - Children of Blood, Scenario III — Blood Money
  - Infected encounter set
---

# Blood Money's setup gathers and sets aside the Infected encounter set

**Q: Children of Blood, Scenario 3. I believe the setup should require the
encounter set "Infected" to be gathered and set aside. Otherwise, at resolution
the card "Blood Blight" does not exist to be earned if necessary.**

A (Alex Werner, FFG Game Rules Specialist):

> We agree that the setup for Children of Blood Scenario 3, Blood Money, should
> have gathered the "Infected" encounter set and set it aside, and are likely to
> give it an official erratum. It's intended that the investigators be able to
> add The Blood Blight to their decks if they meet the conditions outlined in
> the resolutions.

This is an **erratum to Blood Money's setup**, not a change to any card or to
any resolution. All three of Blood Money's resolutions — no resolution,
resolution 1 and resolution 2 — end with "Each investigator who does not have
The Blood Blight weakness and who ended the scenario with 2 or more {blood}
tokens sealed on their investigator becomes the bearer of a set-aside The Blood
Blight weakness," and resolutions 1 and 2 gate the Chosen of Zburamoarte
campaign reward on at least one investigator holding it. The printed setup
never gathered *Infected*, so there was no set-aside copy for those lines to
refer to. Sibling scenarios I (River of Blood) and II (New Horizons) both
gather and set aside *Infected* already.

## Affected cards / systems

- The Blood Blight (13118) — `backend/arkham-api/library/Arkham/Treachery/CardDefs/ChildrenOfBlood/Infected.hs`,
  `backend/arkham-api/library/Arkham/Treachery/Cards/ChildrenOfBlood/Infected/TheBloodBlight.hs`
- Blood Money setup + resolutions — `backend/arkham-api/library/Arkham/Scenario/Scenarios/ChildrenOfBlood/BloodMoney.hs`
- Blood Money setup text — `frontend/src/locales/en/childrenOfBlood/scenarios/bloodMoney.json`

## Implementation status

- **The Blood Blight is earned correctly already — the erratum does not change
  the outcome here.** `DoStep 1 (ScenarioResolution _)`
  (`BloodMoney.hs:334-341`) calls `addCampaignCardToDeck iid ShuffleIn
  Treacheries.theBloodBlight`, which goes through `fetchCard` on a `CardDef`.
  That instance (`Helpers/FetchCard.hs:54-58`) prefers a set-aside copy and
  otherwise **mints one with `genCard`**, so the weakness was always available
  even with *Infected* ungathered. The missing gather was a setup-fidelity gap,
  not a functional one.
- **Setup**: ✏️ `gather Set.Infected` added alongside the other sets, and
  `setAsideEvery $ CardFromEncounterSet Set.Infected` added to the set-aside
  block, matching `setAsideCommon` in New Horizons
  (`NewHorizons.hs:300-305`). The player-facing setup text now lists *Infected*
  among the gathered sets and in the set-aside instruction.
- **Note for anyone reading the piles**: `gather Set.Infected` currently moves
  no cards. `gatherEncounterSet` (`Helpers/EncounterSet.hs:18`) filters on
  `isNothing . cdCardSubType` — "weaknesses are never shuffled into the
  encounter deck" — and The Blood Blight is the *Infected* set's only card and
  is a `weakness`, so the set gathers empty and the following `setAsideEvery`
  finds nothing in `fromGathered`. This is already true of River of Blood
  (`RiverOfBlood.hs:97`) and New Horizons; in every case the resolution's
  `genCard` fallback is what actually delivers the card. The gather/set-aside
  pair is therefore declarative — it keeps the three scenarios' setups
  consistent with the printed (errata'd) text — and nothing depends on a real
  set-aside copy existing.
- **Chosen of Zburamoarte reward**: ⚠️ unrelated pre-existing gap, flagged not
  fixed. Resolutions 1 and 2 read "If at least 1 investigator has The Blood
  Blight weakness in their deck, the investigators have earned the Chosen of
  Zburamoarte campaign reward card. Add this card to the player card pool."
  `DoStep 1` adds the weakness but never grants the reward. It is the last step
  of the campaign, so it only matters to a later campaign's card pool.

## Tests

No behavior changed, so no test was added. The earning path was already
covered by the `genCard` fallback described above.
