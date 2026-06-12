# Enthralling Encore — Integration Notes

- Scenario id: `90094` ("Enthralling Encore", pack `enc`, encounter set `EnthrallingEncore`).
- Required investigator: Lola Hayes (per assignment; note the printed insert prerequisite is
  actually "at least 1 investigator with parallel content" — the resolutions are written
  generically for every investigator, see below). Selection guard + side-story XP cost
  (2 XP per investigator) are orchestrator-owned.
- Campaign log keys added: NONE.
- i18n scope: `enthrallingEncore` (`frontend/src/locales/en/standalone/enthrallingEncore.json`).

## Orchestrator must wire

1. `Arkham/EncounterSet.hs`: append `| EnthrallingEncore` to `data EncounterSet`
   (referenced by my card defs and the `Arkham/Scenario.hs` encounter-set map entry).
2. `Arkham/SideStory.hs`: `"90094" -> 2` (insert: costs each investigator 2 experience).
3. `frontend/src/locales/en/standalone.ts`: import/register `enthrallingEncore.json` under
   scope `enthrallingEncore`.
4. `frontend/src/arkham/data/side-stories.json` + NewCampaign/ContinueCampaign wiring.
5. `backend/arkham-api/data/cards.json`: add 90094 (scenario, double-sided), 90095 (agenda,
   doom 4, double-sided), 90096 (act, double-sided), 90097 (enemy) + card images
   (90094/90094b/90095/90095b/90096/90096b/90097). Encounter-set icon image
   `enthralling-encore.png` does not exist; my `setup.gatherSets` locale entry only embeds
   icons for the reused sets.
6. Optional UI polish: `frontend/src/arkham/components/ScenarioDeck.vue` — add
   `case 'PropsDeck': return imgsrc("player_back.jpg")` (it is a deck of player cards);
   default falls back to the encounter-card back, which still works.
7. The cabal file is hpack-generated; I added modules but did not touch `arkham-api.cabal`
   (regenerates on build).

## Shared files edited (one-line appends only)

- `Arkham/{Agenda,Act,Enemy}/Cards.hs`: import of new `CardDefs.EnthrallingEncore` module +
  def name at end of the aggregation list.
- `Arkham/{Agenda,Act,Enemy}.hs`: registration entry appended at end of
  `allAgendas`/`allActs`/`allEnemies`.
- `Arkham/Scenario.hs`: `("90094", SomeScenario enthrallingEncore)` and
  `("90094", EncounterSet.EnthrallingEncore)` appended.
- `Arkham/Scenario/Deck.hs`: appended `PropsDeck` constructor + `ToDisplay` case ("Props").

## Chaos bags (insert-authoritative; DIFFERS from the shared guide defaults)

Extracted from the insert PDF text layer; symbol glyph codepoints calibrated against the
2021 inserts (whose known mix is Skull x2, Cultist, Tablet, ElderThing, AutoFail, ElderSign).
Enthralling Encore uses NINE symbol tokens at every difficulty: **Skull x3, Cultist x2,
Tablet, ElderThing, AutoFail, ElderSign**, and Hard/Expert have non-standard numeric mixes:

- Easy: +1 +1 0 0 0 -1 -1 -1 -2 -2 + symbols
- Standard: +1 0 0 -1 -1 -1 -2 -2 -3 -4 + symbols
- Hard: 0 0 0 -1 -1 -2 -2 -3 -3 -4 -5 + symbols
- Expert: 0 -1 -1 -2 -2 -3 -3 -4 -4 -5 -6 -8 + symbols

## Implementation decisions / rules calls

- **Measures** are Resource tokens placed on `ScenarioTarget` (the scenario reference);
  the existing scenario-card pool UI already renders them. Skull reads the count
  (X = measures, +1 on Hard/Expert).
- **Props deck** is a new `ScenarioDeckKey` (`PropsDeck`) built at setup from the insert's
  suggested 15 level-0 core-set assets (Physical Training, First Aid, Machete, Old Book of
  Lore, Hyperawareness, Medical Texts, Burglary, Pickpocketing, Hard Knocks, Holy Rosary,
  Scrying, Arcane Studies, Baseball Bat, Rabbit's Foot, Dig Deep). "Search your collection",
  "no duplicates if possible" and "prefer cards not used in investigator decks" are not
  modeled (fixed list).
- Act parley: requires activator at a Private location + non-empty Props deck; cost
  `SameLocationGroupClueCost (PerPlayer 1) (LocationWithTrait Private)`. Reveals (focuses)
  top 3, the activating investigator chooses an investigator, that investigator picks 1 to
  add to hand (owner is set on the card if it had none, so it can be played/discarded
  normally); the other 2 are shuffled and placed on the bottom. Adding to hand does not
  count as a formal "draw" trigger.
- Agenda front parley: `Costs` of five `HandDiscardCost 1` (one per class); effect is
  `automaticallyEvadeEnemy` + `Blank` on the Soloist until end of round (Mind Wipe
  semantics: abilities filtered, outgoing modifiers stripped, traits kept; he is exhausted
  by the evade so Hunter/Forced/Elusive are moot for the round).
- Agenda back (Dirge of Dim Carcosa): place 1 measure; at 5+ measures each investigator
  suffers 1 mental trauma and is defeated (no-resolution -> R2). Otherwise each
  uneliminated investigator tests willpower (3); each failer takes 1 horror and chooses a
  non-permanent, non-weakness asset they control to shuffle into the Props deck
  (`AddToScenarioDeck` + one `ShuffleDeck` afterwards). Tested/failed counts are tracked in
  agenda meta; if failed == tested (and > 0), heal 1[per_investigator] damage from the
  Soloist. Then clues are replenished on revealed locations (`placeCluesUpToClueValue`),
  encounter discard shuffled back in, and the agenda resets to 1a
  (`ResetAgendaDeckToStage 1` + self `RevertAgenda` + doom reset, the WhereIsShe pattern).
- Sinister Soloist: 4 fight / PerPlayer 6 health / 5 evade, 2/2, Victory 2, unique,
  Monster. Elite., keywords Alert/Aloof/Elusive/Hunter (engine handles Alert + Elusive).
  Additional cost modifier `AdditionalPlayCostOf (non-Neutral card) (HorrorCost 1)` applied
  to investigators at his location. Forced: when the enemy phase ends, if ready, 1 direct
  horror to each investigator at his location and connecting locations.
- **Tablet** ("3 or more classes among cards you control"): distinct classes among the five
  player classes (Neutral excluded) across in-play controlled assets/events/skills plus
  hand, deck and discard — per RR "Ownership and Control" (a player controls cards in their
  out-of-play areas). For Lola (3-class deck) this is effectively always on, which appears
  to be the design intent.
- Cultist: E/S — on fail, choose spend 1 clue or take 1 damage (damage forced when no
  clues); H/E — spend 1 clue (if able) and take 1 damage.
- ElderThing: E/S -2, reveal another token if the Soloist is at your location (engine
  `DrawAnotherChaosToken`); H/E -3 or `AutoFailModifier` instead when at his location.
- **Resolutions / swap rewards** (the insert is generic — "each investigator", not just
  Lola): victory-display XP via `allGainXp'` in both resolutions. Then per investigator,
  using a name-keyed table of the 11 parallel investigators with Advanced cards (Roland,
  Daisy, Skids, Agnes, Wendy, Zoey, Rex, Jenny, Jim Culver, Father Mateo, Monterey Jack):
  - R1 (Soloist in victory display): MAY upgrade signature -> advanced OR downgrade
    advanced weakness -> original (plus an explicit "do not swap"); investigators with
    NEITHER option legal earn 2 bonus XP instead.
  - R2 (also reached from no-resolution): MUST upgrade weakness -> advanced OR downgrade
    advanced signature -> original (auto-applied if only one is legal); investigators with
    neither legal suffer 1 mental trauma instead.
  - Lola Hayes and "Ashcan" Pete have only Replacement (not Advanced) signatures, and
    non-parallel investigators have none, so they are always "unable" (R1: +2 XP,
    R2: +1 mental trauma) — implemented literally per the insert wording.
  - Swap mechanism: `RemoveCampaignCardFromDeck` + `AddCampaignCardToDeck DoNotShuffleIn`.
  - "If able" checks: campaign mode consults the campaign's stored decks + campaign story
    cards; pure standalone falls back to in-game zones (deck/hand/discard/in-play/Props
    deck) where the choice is cosmetic anyway.
- Props-deck cards and any investigator assets shuffled into it need no end-of-scenario
  restoration: campaign decks are rebuilt from stored decklists each scenario.
- Setup: gathers Enthralling Encore, Curtain Call, Decay and Filth, Delusions, Hauntings,
  Ghouls, Striking Fear, Rats; removes The Man in the Pallid Mask + Royal Emissary from the
  game; sets the 6 doorway locations aside (Lobby/Backstage reveal abilities place 2 of 3
  each, unchanged from Curtain Call); Theatre/Lobby/Balcony/Backstage in play; everyone
  starts at Theatre (no Lola-specific start, unlike Curtain Call); Soloist enters play at
  (unrevealed) Backstage; original Curtain Call ref/acts/agendas simply unused.

## Uncertainties

- Insert symbol-token mapping (Skull x3 / Cultist x2) is inferred from the PDF's icon-font
  codepoints calibrated against the 2021 inserts; the numeric tokens are verbatim from the
  text layer.
- If a dirge willpower-test failer is defeated by the 1 horror, the asset-to-set-aside
  choice is still offered to them (timing edge; matches the card's simultaneous wording).
- Committed skill cards count as "cards you control" for the Tablet class count.
