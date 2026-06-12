# Relics of the Past — Integration Notes

Challenge scenario for Monterey Jack (90065). Side-story cost: Monterey 3 XP, others 1 XP
(shared infra). Standalone + side-story supported.

## Scenario structure

- Reuses The Doom of Eztli skeleton: exploration deck, Entryway start, DoE encounter sets
  (Agents of Yig, Deadly Traps, Forgotten Ruins, Poison, Serpents, Yig's Venom, Chilling
  Cold, Locked Doors) + Midnight Masks (5 treacheries via `gatherJust`).
- Removed from DoE: original Secret Passage / Ancient Hall / Chamber of Time, Relic of Ages,
  Harbinger of Valusia. Replaced by RoP variants: `secretPassageRelicsOfThePast`,
  `ancientHallRelicsOfThePast`, `innerChamber` (set aside).
- Exploration deck: Grand Chamber, Burial Pit, Underground Ruins, RoP Secret Passage,
  RoP Ancient Hall + Deep Dark + Final Mistake + 3x Pit Viper (+ Locked Door + Entombed on
  Hard/Expert).
- Set aside: Inner Chamber, Vengeant Past, 3x Brood of Yig, 4 Ancient story assets
  (Jade Crocodile, Obsidian Jaguar, Citrine Snake, Turquoise Eagle), Poisoned weaknesses
  (TFA `getSetAsidePoisonedCount`).

## Supplies (scenario-unique)

- Chalk, Compass, Journal, Satchel, Torches — picked at PreScenarioSetup, 1 each (2 if solo),
  stored in scenario meta + investigator supplies via `pickSupply`.
- **Shared-file change**: added `Journal` and `Satchel` variants to
  `Arkham.Campaigns.TheForgottenAge.Supply` (+ `supplyLabel` entries + TFA base.json locale).
  Cost 0, not in prologue list — invisible to TFA campaign flows.
- Picked supplies are removed at resolution (`useSupply`) so they never carry over.
- EDGE (flagged): when played as a side-story inside a TFA campaign, the insert says campaign
  supplies are unavailable during this scenario; picked supplies merge into the same supply
  list so campaign Chalk/Compass/Torches would still satisfy `HasSupply` checks here.

## Agenda 1 → Dweller in the Pit

- Agenda 1 (Something Else Stirs..., 4+1/inv doom) physically flips into the Elite enemy
  Dweller in the Pit (90066b, `doubleSided "90066a"`). Engine: on advance, the Dweller is
  created (spawn engaged with Monterey, fallback lead's location) and the agenda deck moves
  to agenda 2.
- Doom retention on agenda 1 advance via `removeDoomMatchersL` (locations keep doom).
- Dweller: +1/inv health and +1 fight per vengeance point (max +3 fight); after damaging you
  without the Satchel, your Ancients shuffle into the exploration deck.
- Agenda 2 (Guardian of the Relics, 12+1/inv): advance → each uneliminated investigator
  defeated + 1 physical trauma.
- Both agendas: defeated investigator shuffles controlled Ancients into exploration deck.

## Explore

- Story assets (the 4 relics) are legal explore draws (`CardWithType EncounterAssetType`
  added to the explore matcher) = successful exploration with revelation; enemies resolve
  as normal (unsuccessful).

## Chaos tokens (scenario reference)

- Skull: −X = locations with doom (HE: +1).
- Cultist: −1/−2, poisoned → reveal another token.
- Tablet: −3 fail → doom on your location (ES); −4 unconditional doom (HE).
- ElderThing: −3 fail → nearest ready unengaged Serpent moves toward you (ES);
  −5 after test resolves (HE), via `afterSkillTestQuiet`.
- Standalone bags use the insert's custom TFA-style numerics (NOT the shared challenge
  table) + Skull,Skull,Cultist,Tablet,ElderThing,AutoFail,ElderSign — verified against
  insert glyphs at high zoom.

## Resolutions

- No resolution: any resigned → R1, else (all defeated) → R2.
- R1: Monterey XP = combined Victory X of Ancient story assets in the victory display;
  others = Victory X of enemies + locations in display (in-play cleared victory locations
  included per standard rules). Journal-holder resigned → +1 XP to everyone.
  Monterey MAY upgrade Trusty Bullwhip → Advanced or downgrade Advanced Buried Secrets.
- R2: everyone standard victory-display XP (`allGainXp'`). Monterey MUST upgrade Buried
  Secrets → Advanced or downgrade Advanced Trusty Bullwhip (`chooseOrRunOneM`, no skip).
- Swaps use `removeCampaignCardFromDeck` + `addCampaignCardToDeck DoNotShuffleIn`,
  gated on actual deck/story-card ownership (`matchingCardsAlreadyInDeck` +
  `getCampaignStoryCards`) so they work in both standalone and campaign modes.

## Files

- Scenario: `Arkham/Scenario/Scenarios/RelicsOfThePast.hs`, `Arkham/Scenarios/RelicsOfThePast/Helpers.hs`
- Acts: `CrumblingRuin`, `FindTheWayOut`; Agendas: `SomethingElseStirs`, `GuardianOfTheRelics`
- Locations: `SecretPassageRelicsOfThePast`, `InnerChamber`, `AncientHallRelicsOfThePast`
- Assets: `JadeCrocodile`, `ObsidianJaguar`, `CitrineSnake`, `TurquoiseEagle`
- Enemy: `DwellerInThePit`; Treachery: `VengeantPast`
- CardDefs per type + aggregator imports + entity registrations (Act/Agenda/Asset/Enemy/
  Location/Treachery .hs) all included.
- Locale: `frontend/src/locales/en/standalone/relicsOfThePast.json` (+ TFA base.json
  journal/satchel supply entries).
