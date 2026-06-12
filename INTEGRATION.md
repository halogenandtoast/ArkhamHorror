# Red Tide Rising — Integration Notes

- Scenario id: `90041` ("Red Tide Rising"), pack `rtr`, encounter code `red_tide_rising`.
- Required investigator: **Wendy Adams** (matched by title `InvestigatorWithTitle "Wendy Adams"` so parallel 90037 works). No deck prerequisite. Selection guard + side-story XP cost (Wendy 3 / others 1) are NOT implemented here (orchestrator-owned).

## Orchestrator must wire

- `Arkham/EncounterSet.hs`: add `RedTideRising` enum constructor (this branch's code references `Set.RedTideRising` / `RedTideRising` in card defs; it did not exist on my base commit).
- `backend/arkham-api/data/cards.json`: entries for `90041` (scenario), `90042`, `90043` (agendas), `90044` (act), `90045a` + `90045b` (double-sided asset Mysterious Photo, `back_link_id` 90045b).
- `frontend/src/locales/en/standalone.ts`: import + register `redTideRising` (file created at `frontend/src/locales/en/standalone/redTideRising.json`).
- `frontend/src/arkham/data/side-stories.json` + NewCampaign/ContinueCampaign wiring for scenario id `90041`.
- Reused TVoEH card UI strings: the TVoEH/TIC cards in this scenario reference campaign-scoped i18n keys (`campaign.theInnsmouthConspiracy.theVanishingOfElinaHarper.*`, e.g. Innsmouth Square resign tooltip). Ensure those locale chunks are available when playing this standalone.
- arkham-api.cabal: regenerate via hpack (new modules are auto-discovered via package.yaml source-dirs + cards-discover).

## Campaign log keys

None added. The only persistent effects are the advanced-card swaps, done directly via
`RemoveCampaignCardFromDeck` + `AddCampaignCardToDeck DoNotShuffleIn` in the resolutions.

## i18n

Scope `standalone.redTideRising` (backend `Arkham.Scenarios.RedTideRising.Helpers.scenarioI18n`).
Keys: `intro.*`, `setup.*`, `label.*`, `resolutions.{noResolution,resolution1,resolution2}.{title,body}`, `resolutions.xp.{wendy,other}`, `resolutions.label.*` (swap choices), `tokens.{easyStandard,hardExpert}.*`, `specialRules`.

## Shared-file edits (beyond one-line aggregator/Scenario.hs insertions)

The six TVoEH Suspect enemies (BrianBurnhamWantsOut, OtheraGilmanProprietessOfTheHotel,
JoyceLittleBookshopOwner, BarnabasMarshTheChangeIsUponHim, ZadokAllenDrunkAndDisorderly,
RobertFriendlyDisgruntledDockworker) gained a guard on their printed "if no clues remain, add
to the victory display" effect: it is skipped while the enemy has
`ScenarioModifier "victoryRequiresMysteriousPhoto"`. The RTR act applies that modifier (plus
`LoseVictory`, which blocks the defeat-to-victory-display engine path). TVoEH behavior is
unchanged (the modifier is never applied there).

## Rules decisions / judgment calls

- Act 90044 "cannot be added to the victory display except using Mysterious Photo":
  implemented as act modifiers on Suspects (`LoseVictory` + the gate above). Hideouts have no
  self-add effect so only the Photo can add them. A Suspect defeated in this scenario is
  discarded (Innsmouth Jail's "shuffle into Leads deck instead of discarding" still excludes
  them because the implementation checks `not_ EnemyWithVictory` against printed victory).
- XP (both resolutions): each investigator gains victory points of NON-Suspect/Hideout cards
  in the victory display (e.g. Winged One) plus the count formula — Wendy `max 0 (total - 3)`,
  others `max 0 (total - 5)` where `total` = ALL cards in the victory display. Suspect/Hideout
  Victory X is ignored everywhere (including the in-play clueless-location tally, which the
  custom XP code does not use; no base-location in TVoEH has victory, so this only suppresses
  un-photographed Hideouts left in play — judged to be the design intent).
- No-resolution routing: all eliminated and no investigator was defeated (i.e. everyone
  resigned) → R1; otherwise (any defeat) → R2. Mixed resign/defeat is not covered by the
  insert; any defeat routes to R2.
- R1 swap is may (upgrade Wendy's Amulet 01014→90039 OR downgrade Abandoned and Alone
  90040→01015 OR skip) unless Wendy was defeated, in which case (and in R2) it is must
  (upgrade Abandoned and Alone 01015→90040 OR downgrade Wendy's Amulet 90039→01014); only
  options whose source card the Wendy player owns are offered, auto-applied when forced with
  a single legal option, skipped when none. Offered in pure standalone too (harmless).
- Mysterious Photo side B "take an investigate action at a Hideout location": implemented as
  a free basic investigate of your own location, only while at a Hideout (no remote
  investigation). Side A parley grant offers any performable parley ability on a Suspect
  enemy (their own abilities are OnSameLocation-restricted), action cost ignored.
- Photo side B hideout costs are static per printed clue value (PerPlayer 1: Innsmouth Jail,
  Shoreward Slums, Esoteric Order of Dagon; PerPlayer 2: Sawbone Alley, The House on Water
  Street, New Church Green), +1 per investigator during agenda 2 — encoded as four
  criteria-gated ability variants (at most one visible at a time). Suspect side likewise has
  agenda-1/agenda-2 variants.
- Agenda 2's "+1 [per_investigator] clue value" also covers hideouts ENTERING play during
  agenda 2 (forced ability places 1 [per] extra clues); already-placed clues on hideouts in
  play are not retroactively adjusted (clue value ≠ clues on it).
- "Moving each enemy and investigator there to a connecting location" (Photo side B): each
  moved investigator picks their own destination; the lead investigator picks for enemies.
- Photo "cannot leave play": forced when-leaves-play ability cancels removal while an active
  (uneliminated) Wendy exists; when Wendy is eliminated it leaves play normally. "Each
  surviving investigator should resign" is advisory and not enforced.
- Setup draw of the first Lead replicates the revelation: a Suspect spawns at its printed
  spawn location with 1 [per] clues; a Hideout enters play (clues placed by the engine).
  The setup-time random Monster (4+ adjusted investigators) is shuffled into the Leads deck
  built from the remaining 11 leads, so it can never be the initial draw (matches insert
  ordering).
- Chaos bag: insert glyphs verified at high zoom — 8 symbols per difficulty:
  Skull, Skull, Cultist, Cultist, Tablet, ElderThing, AutoFail, ElderSign (the guide's
  7-symbol table was missing the second Cultist). Module token lists corrected.

## Files created

- backend: `Arkham/Scenario/Scenarios/RedTideRising.hs`, `Arkham/Scenarios/RedTideRising/Helpers.hs`,
  `Arkham/Agenda/CardDefs/RedTideRising.hs`, `Arkham/Agenda/Cards/TheNewGirl.hs`,
  `Arkham/Agenda/Cards/TrailGoesCold.hs`, `Arkham/Act/CardDefs/RedTideRising.hs`,
  `Arkham/Act/Cards/SearchingForDad.hs`, `Arkham/Asset/Cards/RedTideRising.hs`,
  `Arkham/Asset/Assets/MysteriousPhoto.hs`, `Arkham/Asset/Assets/MysteriousPhotoBack.hs`
- frontend: `frontend/src/locales/en/standalone/redTideRising.json`
