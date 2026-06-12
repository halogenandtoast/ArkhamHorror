# Read or Die — Integration Notes

- **Scenario id**: `90004` (scenario reference card), name "Read or Die", `pack_code "rod"`, encounter code `read_or_die`.
- **Required investigator**: Daisy Walker (original `01002` or parallel `90001`; matched by title in-code). Deck prerequisite: at least 4 non-weakness Tome assets (guard is orchestrator-owned; the scenario itself does not enforce it).
- **Side-story XP cost** (orchestrator-owned): Daisy Walker 3 XP, each other investigator 1 XP.

## Orchestrator must wire (files I did not touch)

- `Arkham/EncounterSet.hs`: add enum variant `ReadOrDie`. **It is NOT present on this branch's base commit** despite the shared guide saying it would be; all my card defs and `Arkham/Scenario.hs` entries reference `ReadOrDie` and will not compile until it is added.
- `backend/arkham-api/data/cards.json`: add cards `90004` (scenario), `90005` (agenda, doom 12), `90006` (act), `90007` (enemy: fight 2, health 3, evade 2, damage 1, horror 1, traits Monster. Geist. Elite., Hunter) plus back sides as appropriate (source JSON: arkham.build `read-or-die.json`).
- `Arkham/SideStory.hs` / `Arkham/Campaign/Runner.hs`: side-story insertion flow (play-once-per-campaign, XP costs, Daisy-required guard).
- `frontend/src/arkham/data/side-stories.json`: entry for `90004`.
- `frontend/src/locales/en/standalone.ts`: `import readOrDie from '@/locales/en/standalone/readOrDie.json'` + add `readOrDie` to the exported map.
- `frontend/src/arkham/views/NewCampaign.vue` / `ContinueCampaign.vue`: selection UI.
- Encounter set icon: locale `setup.gatherSets` does not reference a `read-or-die.png` image (the set list names it textually); the other nine set icons referenced all already exist on the asset server.
- Card images for 90004–90007 on the image CDN.

## Campaign log keys

None added. The resolutions need no persistent keys: XP is granted in-resolution and the signature-card swaps use `RemoveCampaignCardFromDeck`/`AddCampaignCardToDeck` directly.

## i18n

- Scope: `readOrDie` (file `frontend/src/locales/en/standalone/readOrDie.json`, accessed via `standaloneI18n "readOrDie"`, i.e. keys live under `standalone.readOrDie.*`).
- Uses global keys `shuffleRemainder` and `readyToBegin` from `base.json` (unscoped).

## Files added

- `Arkham/Scenario/Scenarios/ReadOrDie.hs`, `Arkham/Scenarios/ReadOrDie/Helpers.hs`
- `Arkham/Agenda/CardDefs/ReadOrDie.hs` (`mortalInquiry` 90005), `Arkham/Agenda/Cards/MortalInquiry.hs`
- `Arkham/Act/CardDefs/ReadOrDie.hs` (`speedReading` 90006), `Arkham/Act/Cards/SpeedReading.hs`
- `Arkham/Enemy/CardDefs/ReadOrDie.hs` (`namerOfTheDead` 90007), `Arkham/Enemy/Cards/NamerOfTheDead.hs`
- One-line insertions in `Arkham/{Agenda,Act,Enemy}/Cards.hs` (import + def list) and `Arkham/Scenario.hs` (`("90004", SomeScenario readOrDie)` and `("90004", EncounterSet.ReadOrDie)`).

## Rules decisions / judgment calls

- **Tome placement order**: the insert's wording ("1 beneath each location except Miskatonic Quad … remaining beneath Orne Library") conflicts with its own example (12 tomes → 6 beneath Orne Library) unless Orne Library is treated purely as the remainder sink. Both readings produce identical final states for every tome count because Orne Library is the least-far location; implemented as: 1 facedown beneath each of the six non-Quad, non-Orne locations in descending engine-computed distance from Orne Library (random tie-break via pre-shuffle), all remaining beneath Orne Library.
- **Tome removal timing**: done in `PreScenarioSetup` (before opening hands are drawn) so set-aside tomes can never be in Daisy's opening hand; they are held in the scenario set-aside zone until `Setup` distributes them.
- **Facedown player cards beneath locations**: uses `PlaceUnderneath`; engine renders player cards under a location as a count with a "show" button, so card identities are technically inspectable in the UI (engine-wide behavior, accepted).
- **"Ignore Forced abilities that would put Dormitories/Faculty Offices/Alchemy Labs into play"**: scenario-level `CannotTriggerAbilityMatching (AbilityIsForcedAbility <> AbilityIs <location> 1)` on everyone for Student Union/Science Building/Administration Building ability 1. This is required (Science Building's Forced would otherwise try to place the removed-from-game Alchemy Labs). Faculty Offices' reveal-Forced (spawn a Humanoid) is *not* suppressed — it doesn't put a location into play.
- **"Ignore all Objectives on locations"** (act): the only location Objectives in the card pool are Dormitories ability 1 (→R2) and Faculty Offices ability 2 (→R1); suppressed via `CannotTriggerAbilityMatching` from the act. No generic "Objective" ability matcher exists in the engine.
- **Namer of the Dead defeat replacement**: `EnemyWouldBeDefeated` forced ability → cancel defeat, heal all damage, exhaust, `enemyMoveTo` Orne Library. If it is "moved" to the location it is already at while engaged, engine semantics keep it engaged (exhausted) — consistent with RR enemy movement.
- **Parley**: requires Daisy (by title), same location, and 4+ non-weakness Tome assets she controls; test difficulty `max 0 (18 − 2 × Tome assets controlled, including weaknesses)` per card text ("each Tome asset you control"). Success advances the act (act back: →R1).
- **Agenda's Armitage grant**: "2 additional hand slots (Tome only)" implemented as two `TraitRestrictedSlot` hand slots sourced from the *Armitage asset* and granted on `TakeControlOfAsset`, so the engine auto-removes them if Armitage leaves play. If control of Armitage were somehow transferred mid-game the old controller would keep the slots (no in-scenario effect can do this).
- **Jazz's granted move action**: proxied ability on the agenda, usable while Jazz is uncontrolled by an investigator at his location ("up to 3 connections away" = distance ≤ 3, Miskatonic, not his current location). When an investigator controls Jazz (via his own parley) the granted ability is moot (he's in a play area), so it requires `Uncontrolled`.
- **Dr. Henry Armitage**: if `getOwner` (campaign story cards) finds an owner, the card is taken from that investigator's deck (or hand), the owner's deck is shuffled (deck search), and the gathered Armitage's Fate copy is excluded; otherwise the gathered copy is used via `beginWithStoryAsset`.
- **Daisy's XP**: `max(standard victory-display XP, Tome assets Daisy controls in play at resolution time)` implemented as standard `allGainXp'` plus a Daisy-only bonus (`gainXp` with i18n key `resolutions.xp.tomes`) for the difference. If Daisy was defeated her assets are out of play, so the tome count is 0 (matches "she had in play at the end of the game").
- **Swaps** (only offered when the relevant version is in Daisy's pool: deck + hand + discard + in-play owned assets):
  - R1 (may): upgrade Daisy's Tote Bag (01008 → 90002) OR downgrade advanced Necronomicon (90003 → 01009) OR do not swap.
  - R2 (must, also reached from no-resolution): upgrade The Necronomicon (01009 → 90003) OR downgrade advanced Tote Bag (90002 → 01008); auto-applies if only one is legal, does nothing if neither is.
  - Swaps use `RemoveCampaignCardFromDeck` + `AddCampaignCardToDeck _ DoNotShuffleIn`; shown in pure standalone too (harmless).
- **Chaos bag**: four difficulty lists per the insert (symbol mix: 2× Skull, Cultist, Tablet, ElderThing, AutoFail, ElderSign — matches the shared guide).
- **Skull**: −X, X = Tome assets Daisy controls (hard/expert +1); Daisy eliminated → 0 (or 1 on hard/expert).
- **Cultist**: reveal another token; on fail discard top 2 (easy/standard) / 3 (hard/expert) cards of your deck.
- **Tablet**: −2/−3; on a fight/evade test targeting the Namer, passing by less than 2/3 (or failing) causes it to attack you.
- **Elder Thing**: −3/−5; on fail Daisy takes 1 horror (whoever was testing).

## Uncertainties

- The Read or Die encounter set icon and the `90004b`–`90007b` back-side data are not in my per-scenario JSON; backs were sourced from `.claude/data/cards.json` (`real_back_text` on 90005/90006).
- Solo Daisy: Miskatonic Quad starts in play unrevealed with nobody there (no "other investigators"); deemed correct per setup text.
- The unrevealed Dormitories/Faculty Offices stay locked ("cannot move into") exactly as in Extracurricular Activity; Jazz Mulligan's blanking of unrevealed Miskatonic locations (while controlled, active investigator) is the intended unlock, with the agenda's move-Jazz action letting investigators reposition him.
