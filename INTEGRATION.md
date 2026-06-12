# Laid to Rest — integration notes

- Scenario id: `90054`, name "Laid to Rest", pack `ltr`, encounter set `LaidToRest`.
- Required investigator: Jim Culver (works with `02004` and parallel `90049` — matched by
  title "Jim Culver"). Deck prerequisite: the Jim player must bring the 10-card spirit deck
  (side deck) per parallel Jim's deckbuilding; The Beyond reads `InvestigatorSideDeck` and the
  scenario shuffles 4 random Heretics into it during setup.
- Campaign log keys: none added.
- i18n: `frontend/src/locales/en/standalone/laidToRest.json`, scope `laidToRest`
  (needs the usual one-line registration in `frontend/src/locales/en/standalone.ts`).

## Orchestrator must wire (files I was told not to touch)

- `Arkham/EncounterSet.hs`: add `LaidToRest` enum variant. The guide said the challenge
  variants were pre-added on the base commit, but this worktree's base does NOT have them.
  Referenced from `Arkham/{Enemy,Agenda,Act}/CardDefs/LaidToRest.hs` and `Arkham/Scenario.hs`.
- `backend/arkham-api/data/cards.json` + frontend data: cards `90054`, `90055`, `90056`,
  `90057a`/`90057b`, `90058` (x4); side-stories.json entry; NewCampaign/ContinueCampaign,
  Jim Culver selection guard, side-story XP (Jim 3 / others 1).

## Shared file modified (no other agent should touch it)

`Arkham/Asset/Assets/TheBeyondBleakNetherworld.hs` — needed so Heretic (encounter enemy)
cards can live in the spirit deck per the insert:

- Attach forced effect + ElderSign "attach top card" now handle `EnemyType` cards
  (created attached to The Beyond, like Vengeful Shade). Attaching an `EnemyType` spirit via
  the forced effect also triggers Vengeful Shade's attack.
- The back-side judgment now includes non-weakness ENEMY spirits; a "discarded" enemy spirit
  is removed and its card returns to the bottom of the spirit deck (insert rule: spirits are
  always discarded to the bottom of the spirit deck).
- `Meta` gained `selectedEnemySpirit :: Maybe EnemyId` (optional aeson field; old saves parse).
- New message contract (sent via plain `ScenarioSpecific`, helpers in
  `Arkham/Scenarios/LaidToRest/Helpers.hs`):
  - `"theBeyond:addToSpiritDeck"` payload `[Card]` — shuffle cards into the spirit deck (setup).
  - `"theBeyond:banishTop"` — move top card of the spirit deck to the bottom (agenda parley).
  - `"theBeyond:returnSpirit"` payload `Card` — put a card on the bottom of the spirit deck.

Patrol: used the existing `Patrol LocationMatcher` keyword/engine handling unchanged
(Jean: `LocationWithCardsUnderneath (HasCard (CardWithTitle "Ravenous Spirit"))`;
Ravenous Spirit: `LocationWithEnemy (EnemyWithTitle "Jean Devereux")`). No engine edits, so no
conflict with Bad Blood.

## Rules judgment calls / uncertainties

1. Agenda parley "You banish a spirit. Discard the top card of the Spirit deck.": implemented
   as moving the top card of the spirit deck to the bottom (90052b reminder: "Spirits are
   always discarded to the bottom of the spirit deck"). Heretics banished this way stay in
   rotation, so the act objective can't be bricked.
2. Act back "Heretic enemy still in The Beyond": counted as Heretics attached to The Beyond
   plus Heretics still in the spirit deck, computed by subtraction:
   `4 - (Heretic enemies in play not attached to The Beyond) - (Unfinished Business stories in
   play) - (Unfinished Business in victory display)`, min 0; trauma is `max 1` of that.
3. The Beyond's back judges Heretic spirits too (card says "each non-weakness spirit").
4. Ravenous Spirit forced effect: if the location is already Spectral the flip fizzles but
   investigators there still resolve each haunted ability; triggers on spawn and on move
   (`oneOf [EnemyEnters, EnemySpawns]`, single fire per window batch).
5. Cultist token "cards attached to The Beyond" counts attached assets + attached enemies
   (includes Vengeful Shade if attached).
6. Easy/Standard Elder Thing "reveal another token" resolves at token-resolution time
   (`drawAnotherChaosToken`); Hard/Expert auto-fail is the token's value (`AutoFailModifier`)
   when a Geist enemy is at your location.
7. Jean's parley: drawing the facedown Ravenous Spirit is done by the parleying investigator
   (spawns at their/Jean's location); the chosen Heretic is flipped for Jim via the existing
   `hereticRunner` flip (ends as Unfinished Business in Jim's threat area). Ability is gated on
   Jim being in play, a Heretic attached to The Beyond, and a facedown card beneath Jean's
   location.
8. Act/agenda "advance the act" forced effects push `AdvanceAct _ _ #other`; the act tracks
   `advancedViaObjective` in metadata to pick the R1 vs R2 back-side branch.
9. "Each other investigator … is defeated and suffers 1 mental trauma" pushes
   `SufferTrauma` + `InvestigatorDefeated` (TimeMarchesOn pattern); if that eliminates everyone
   the engine's no-remaining-investigators handler clears the queue and runs NoResolution,
   which this scenario routes to R2 — same result, no double resolution.
10. XP (both resolutions): Jim = combined Victory X of enemy cards in the victory display;
    others = combined Victory X of location cards in the victory display plus revealed,
    clueless victory locations in play (standard end-of-game rule). Custom `XpBreakdown` +
    `GainXP` per investigator; defeated/eliminated investigators included
    (`IncludeEliminated … InvestigatorCanGainXp`).
11. Swap rewards: R1 — Jim MAY upgrade Jim's Trumpet (02012→90050) OR downgrade advanced
    Final Rhapsody (90051→02013), always offered "do not swap". R2 — Jim MUST upgrade Final
    Rhapsody (02013→90051) OR downgrade advanced Jim's Trumpet (90050→02012); auto-applies if
    only one is legal, nothing if neither. Deck presence checked against campaign decks +
    campaign story cards (works in standalone via scenario player decks).
12. Group limit "once per round per location" on the act's flip ability uses act metadata reset
    on `EndRound` (same as In Pursuit of the Living).
13. Setup places 1 [per_investigator] clues on every location (all start clueless; printed clue
    values are 0); the agenda back replenishes every location up to 1 [per_investigator].
