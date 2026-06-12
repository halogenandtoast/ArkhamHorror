# All or Nothing — Integration Notes

- **Scenario id**: `90011` ("All or Nothing", pack `aon`, encounter set `AllOrNothing`, EncounterSet variant `AllOrNothing`).
- **Required investigator**: "Skids" O'Toole (matched by title `"Skids" O'Toole`, so original `01003` and parallel `90008` both work). No other deck prerequisite. Selection guard + side-story XP cost (Skids 3 XP, each other investigator 1 XP) are NOT implemented here — central. Note `Arkham/SideStory.hs` `getSideStoryCost` has no `"90011"` entry and cannot express per-investigator costs.

## Campaign log keys added

- `AllOrNothingBonusResources :: Int` (count, 1–6), recorded in Resolution 1 only when ≥ 10 resources were on Act 2a. Consumption (central): **the NEXT scenario only** — "Skids" O'Toole begins play with that many additional resources during setup, then clear the record. (40+/50+/60+ tiers also grant Skids 1/2/3 bonus XP — granted immediately in the resolution, not recorded.)

## Files created

- `Arkham/Scenario/Scenarios/AllOrNothing.hs`, `Arkham/Scenarios/AllOrNothing/Helpers.hs`
- `Arkham/Agenda/CardDefs/AllOrNothing.hs` + `Arkham/Agenda/Cards/EyesAllAroundYou.hs` (90012, doom 11)
- `Arkham/Act/CardDefs/AllOrNothing.hs` + `Arkham/Act/Cards/PlayingCards.hs` (90013), `Arkham/Act/Cards/HotOnYourTail.hs` (90014)
- `Arkham/Enemy/CardDefs/AllOrNothing.hs` + `Arkham/Enemy/Cards/SiobhanRiley.hs` (90015), `Arkham/Enemy/Cards/CloverClubBouncer.hs` (90016)
- `frontend/src/locales/en/standalone/allOrNothing.json`
- One-line insertions: `Arkham/{Agenda,Act,Enemy}/Cards.hs` (import + def names at list end), `Arkham/Scenario.hs` (both maps), `Arkham/CampaignLogKey.hs` (new constructor at enum end).

## Orchestrator must wire (files I did not touch)

- `Arkham/EncounterSet.hs`: the guide says the challenge variants were pre-added, but **they are not on this base commit**. Add (suggested, in guide order, at enum end): `ReadOrDie | AllOrNothing | BadBlood | ByTheBook | RedTideRising | LaidToRest | RelicsOfThePast | EnthrallingEncore`. Nothing here compiles without `AllOrNothing`.
- `backend/arkham-api/data/cards.json`: add cards 90011–90016.
- `frontend/src/locales/en/standalone.ts`: register `allOrNothing` (scope `standalone.allOrNothing`).
- `frontend/src/arkham/data/side-stories.json`: add the scenario (4 difficulties; standalone chaos bags are in the scenario module).
- Campaign/Runner: consume `AllOrNothingBonusResources` per above.
- cabal: new modules rely on hpack regeneration from `package.yaml` (`source-dirs: library`); checked-in `arkham-api.cabal` not hand-edited.

## Implementation decisions / judgment calls

- **Chaos tokens** (per 90011, both sides): skull −1/clue (E/S) or −2/clue (H/E) of the revealing investigator; cultist −2/−4, doubled to −4/−8 if the revealing investigator has 10+ resources; tablet −2 with "lose 3 resources" **on fail** (E/S) vs −3 with "lose 3 resources" **on reveal** (H/E — printed text differs); elder thing −3/−5, on fail Skids (if in play) takes 1 horror.
- **Agenda 90012**: action ability uses `ClueCostX` (≥1 clue), gain 5 resources per clue paid; only usable in Clover Club Cardroom. Forced "Skids defeated → R2" uses `InvestigatorDefeated #when` (matches Vengeance/BrethrenOfAsh precedent; #when is required for the title matcher to see the not-yet-eliminated investigator). Agenda back uses the HisDomain `SetNoRemainingInvestigatorsHandler` pattern: each non-resigned investigator suffers 1 physical trauma and is defeated, then the no-remaining handler routes to R1 if Skids resigned, else R2 (avoids the generic NoResolution queue-clear race).
- **Act 1 (90013)**: "Investigators cannot resign" = `CannotPerformAction (IsAction #resign)` on all investigators (La Bella Luna's resign action is blocked); Pit Boss gains Aloof via `AddKeyword`; Forced doom on Criminal-enemy defeat uses `placeDoomOnAgenda 1` (no advancement check); objective is a forced `RoundEnds #when` requiring **a single investigator** with 15+ resources (per card text, not group total). Act 1 back spawns set-aside Siobhan Riley at La Bella Luna and one set-aside Bouncer engaged with each (uneliminated) investigator (zip — extra bouncers stay set aside).
- **Act 2 (90014)**: Criminal enemies gain Hunter and `HealthModifier 1`. Forced on resign (#when, before elimination): the resigning investigator loses all resources (`LoseResources`) and the amount is accumulated in scenario meta key `actResources` (via a `ScenarioSpecific "placeResourcesOnAct"` message). The count is **not** visually represented as tokens on the act card (Act entities have no resource-token support); Resolution 1's story text reports the total. Objective: `AllUndefeatedInvestigatorsResigned` → advance → R1. Resolution 1 reads `actResources` even when reached via the agenda back (resources placed before the agenda flipped still count — matches "number of resources on Act 2a").
- **Siobhan Riley (90015)**: 5/6/2, 1 dmg/1 horror, unique, Victory 1, Humanoid.Criminal.Elite. Engage-forced: lose `resources div 5`. Second forced restricted to `OnSameLocation <> DuringPhase #investigation`, window `GainsResources #after You AnySource (atLeast 1)`; readies (if exhausted), engages, attacks (mirrors Clover Club Pit Boss).
- **Clover Club Bouncer (90016)**: 1/3/1, 2 dmg, Victory 0 (both card-data sources list Victory 0; modeled as `cdVictoryPoints = Just 0`). Stat boost +1 fight/+1 evade per 5 resources, computed from the **maximum** resources among currently engaged investigators (exact for the normal single-engagement case; the engine cannot express per-attacker stats). Parley: action, `ResourceCost 3`, test intellect (2), restricted to same location; success = disengage from that investigator + exhaust + `DoesNotReadyDuringUpkeep` for the next upkeep phase.
- **Resolutions**: NoResolution → R2 (per insert). R1: victory-display XP for all; tier table applied at the highest met threshold (10/20/30 → record 1/2/3 resources; 40/50/60 → +1/2/3 bonus XP to Skids now, record 4/5/6 resources); then Skids **may** upgrade On the Lam (01010→90009) or downgrade advanced Hospital Debts (90010→01011) or do nothing. R2: victory-display XP; Skids **must** upgrade Hospital Debts (01011→90010) or downgrade advanced On the Lam (90009→01010); single legal option auto-applies (`chooseOrRunOneM`), nothing happens if neither is legal. Swap legality checks campaign decks (`CampaignDecks`/`ScenarioPlayerDecks`, incl. alternate art codes 01510/01511) plus campaign story cards; swaps use `RemoveCampaignCardFromDeck` + `AddCampaignCardToDeck _ DoNotShuffleIn`.

## Not implemented / caveats

- **Optional "Return to The House Always Wins" variant** (insert sidebar: new Clover Club Lounge, set-aside Clover Club Stage, RtTHAW treacheries, remember "Skids has cheated"): intentionally NOT implemented — base scenario only.
- La Bella Luna / Back Hall Doorway resign-ability tooltips reuse the Dunwich `theHouseAlwaysWins` i18n scope; in pure standalone play those keys may not be loaded by the frontend (cosmetic).
- The 1-physical-trauma on the agenda back applies only to investigators still in play (already-defeated investigators received standard defeat consequences) — matches The Exit (Fortune and Folly) precedent for identical wording.
