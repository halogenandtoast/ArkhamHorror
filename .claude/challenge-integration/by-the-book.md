# By the Book — Integration Notes

## Scenario
- Scenario id: `90032` ("By the Book"), pack `btb`, encounter code `by_the_book`, EncounterSet variant `EncounterSet.ByTheBook`.
- Required investigator: Roland Banks (original or parallel — all in-scenario references match by `InvestigatorWithTitle "Roland Banks"`). Selection guard + side-story XP cost (3 for Roland, 1 for others) are NOT implemented here (orchestrator owns).
- Module: `Arkham.Scenario.Scenarios.ByTheBook`; helpers in `Arkham.Scenarios.ByTheBook.Helpers`.

## Orchestrator must provide
- `EncounterSet.ByTheBook` variant in `Arkham/EncounterSet.hs` (this worktree's base commit does NOT have it; all card defs and `Arkham/Scenario.hs` reference it).
- `SideStory.hs` / campaign-insertion / `frontend/src/arkham/data/side-stories.json` entry for `90032`.
- `frontend/src/locales/en/standalone.ts`: `import byTheBook from '@/locales/en/standalone/byTheBook.json'` + add `byTheBook` to the export map.
- `backend/arkham-api/data/cards.json` entries for 90032/90033a/90033b/90034/90035/90036 if validation requires them.
- Frontend campaign-log label for the new `ByTheBookBonusCards` key if it should render nicely.

## Campaign log keys added
- `ByTheBookBonusCards` (appended at end of `CampaignLogKey` enum), written via `recordCount ByTheBookBonusCards n` in Resolution 1 only, with n = 1 (4–5 Cultists in victory display), 2 (6–7), or 3 (8–9 or 10). Not recorded when below 4. Consumption (central): the NEXT scenario only — Roland Banks draws that many additional cards during setup (opening hand), then clear the record. Resolution 2 records nothing.
- Resolution 1 with 10 Cultists in the victory display additionally lets Roland choose a non-symbol (numeric) chaos token to remove for the remainder of the campaign — implemented in-scenario via `chooseOneM` over the bag's numeric faces pushing `RemoveChaosToken face` (campaign runner persists). Nothing for the orchestrator to do here.

## i18n
- Scope: `standalone.byTheBook` (backend `standaloneI18n "byTheBook"`); file `frontend/src/locales/en/standalone/byTheBook.json`.

## Key implementation decisions / rules calls
- 90033b (the back of agenda 1) is the enemy **Mr. Grey** (unique, Humanoid. Cultist. Elite., fight 3, health 3 + 2/investigator via `HealthModifier`, evade 2, 1/1 damage, Hunter, Victory 1, Spawn engaged with Roland Banks). Mirrors core's Predator or Prey?/The Masked Hunter (01121a/01121b) pattern: agenda 1 advance spawns him via `createEnemyCard_`. "(Mr. Grey, if able)" on agenda 2b refers to this card, NOT The Masked Hunter.
- The Masked Hunter is never used: it is the back of the original Midnight Masks agenda 1, which the insert removes from the game. The encounter deck is exactly the insert's 24 cards (Midnight Masks treacheries 5, Agents of Shub-Niggurath 4, Chilling Cold 4, Nightgaunts 4, Striking Fear 7). Your House and Mysterious Chanting are simply never gathered/placed (Dark Cult is pulled via `gatherEncounterSet`, only its 4 enemies are used).
- Conspirators: 9 Cultist enemies (Cult of Umôrdhoth 5 + Dark Cult 4) shuffled, set facedown, one `placeUnderneath` each of the 9 locations. Act ability flips the one under your location with `setFacedown False` + `obtainCard` + `createEnemy_ card iid` (put into play engaged with the activating investigator, overriding printed spawn instructions).
- "Heal damage from that enemy until it has 1 remaining health, instead": forced on `EnemyWouldBeDefeated #when (NonWeaknessEnemy <> Cultist)` on both agendas → `cancelEnemyDefeat` + heal (damage − (modifiedHealth − 1)). Damage is already applied when the window fires (verified against `CheckDefeated` in Enemy/Runner).
- Police station parley: fast ability, criteria require you have ≥1 clue and a non-weakness Cultist here; the handler offers only enemies whose remaining health ≤ your clues (ability-criteria can't express the dynamic cost, so an unaffordable activation is a visible-but-no-op button in rare cases; `chooseOneM` skips empty).
- Police station connections ("connected to Rivertown, Downtown, and Easttown, and vice versa") via `ConnectedToWhen` modifiers in both directions, matching by location title so either Downtown variant works.
- No-resolution routing: Roland resigned → R1 (insert); otherwise → R2 (judgment call — the insert is silent; Roland's defeat already forces R2 directly, so the leftover case is treated as failure).
- Agenda 2b ("Time Has Run Out"): removes Mr. Grey from the victory display if present (via `obtainCard`, which scrubs the victory display; the card is then nowhere = removed from game); otherwise the lead chooses a victory-display Cultist to remove; then R1. This correctly suppresses the R1 Mr.-Grey swap reward and reduces XP/Cultist counts.
- Custom XP (both resolutions): Roland = combined Victory X of enemies in the victory display; each other investigator = combined Victory X of locations in the victory display (victory-display location cards + in-play revealed clueless locations, mirroring `getInitialVictory`). Per-investigator `XPModifier`s applied like `getXpWithBonus`; `ReportXp` breakdown emitted. Locations CAN yield XP (Northside, Downtown: Arkham Asylum, Graveyard have Victory 1).
- Advanced swaps use `RemoveCampaignCardFromDeck` + `AddCampaignCardToDeck iid DoNotShuffleIn`:
  - R1 (only if Mr. Grey in victory display): Roland MAY upgrade Roland's .38 Special (01006 → 90030) or downgrade advanced Cover Up (90031 → 01007); "do not swap" always offered; each option only when the source card is owned.
  - R2: Roland MUST upgrade Cover Up (01007 → 90031) or downgrade advanced .38 Special (90030 → 01006); choice only when both legal, auto-applied when one, no-op when neither.
  - Ownership check: `selectAny (basic (cardIs def) <> OwnedBy (IncludeEliminated roland))` over the game's card pool (covers deck/hand/play/discard in both campaign and standalone play).
- Chaos tokens 90032: skull −X = victory-display Cultist count (E/S capped at 5; H/E = 1 + count, uncapped); cultist −2/−3 reveals an additional token if a Cultist is engaged with you; tablet −3/−4 readies each exhausted Cultist engaged with you; elder thing −4/−5 deals Roland 1 damage on a failed test (skipped if Roland eliminated).
- Standalone chaos bags: the four per-difficulty bags from the insert (Easy bag differs from Standard; matches the shared challenge-scenario spec).

## Not implemented (intentional)
- Return to the Cult of Umôrdhoth optional conspirator variant (insert sidebar): skipped per instructions.
- Regulation abilities sidebar: rules text only; the Directive assets (90025–90029) already exist and are out of scope.
- Edge case: if agenda 1 advances after Roland left play (resigned), Mr. Grey spawns at the lead investigator's location instead of engaged (spawn instruction impossible; keeps him capturable so the act objective stays reachable).
