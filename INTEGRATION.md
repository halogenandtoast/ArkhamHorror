# Bad Blood — Integration Notes

- Scenario id: `90020` ("Bad Blood"), pack `bad`, encounter code `bad_blood`.
- Required investigator: Agnes Baker (matched by `InvestigatorWithTitle "Agnes Baker"`, so both
  original 01004 and parallel 90017 work). Selection guard + side-story XP cost (Agnes 3 XP,
  others 1 XP) are NOT implemented here — orchestrator wires both (`Arkham/SideStory.hs` needs a
  `"90020" -> ...` entry; per the insert the cost is asymmetric: 3 for Agnes, 1 for each other
  investigator — `getSideStoryCost` currently only supports a flat cost).
- Campaign log keys: none added. Memory tallies are scenario-internal meta
  (`Arkham.Scenarios.BadBlood.Meta.Meta { agnesMemories, elspethMemories }`, stored via
  `metaL`/`SetScenarioMeta`-equivalent attrs update, initialized to 0/0 in the constructor).
- i18n scope: `badBlood` (file `frontend/src/locales/en/standalone/badBlood.json`); needs the
  usual one-liner wiring in `frontend/src/locales/en/standalone.ts` plus
  `frontend/src/arkham/data/side-stories.json` / NewCampaign entries (orchestrator-owned).
- Orchestrator must add `BadBlood` to `Arkham/EncounterSet.hs` (enum variant; display name
  "Bad Blood") — code in this branch references `EncounterSet.BadBlood` and will not compile
  until it exists. Cards 90020–90023(b) also need adding to `backend/arkham-api/data/cards.json`
  for validation/frontend card display. `arkham-api.cabal` regenerates via hpack (no manual edit).

## Files

- `Arkham/Scenario/Scenarios/BadBlood.hs`, `Arkham/Scenarios/BadBlood/{Meta,Helpers}.hs`
- `Arkham/{Act,Agenda,Enemy}/CardDefs/BadBlood.hs` (defs) +
  `Arkham/Act/Cards/AWalkDownMemoryLane.hs` (90022), `Arkham/Agenda/Cards/HyperboreanBlood.hs`
  (90021, doom 8), `Arkham/Enemy/Cards/ElspethBaudin.hs` (90023)
- One-line insertions: `Arkham/{Act,Agenda,Enemy}/Cards.hs` (import + def at end of list),
  `Arkham/{Act,Agenda,Enemy}.hs` (lookup map entry at end), `Arkham/Scenario.hs` (both maps).

## Mechanics / rules decisions

- Memories = 1 resource token on each of the 9 locations at setup. Collected memories are
  tracked per-collector in scenario meta; not represented as physical tokens once collected.
  Game-log `send` messages announce each collection with running totals.
- Agnes collects via act 1a fast ability (`GroupClueCost (PerPlayer 2)` at Agnes's location,
  requires a memory there). Elspeth collects via the agenda Forced chain.
- Chaos tokens "placed on locations" use the seal mechanism (`SealChaosToken` +
  `SealedChaosToken ... (LocationTarget)`); returning them to the bag is `UnsealChaosToken`.
  Combined value uses each face's CURRENT scenario token value (abs, sign ignored), with
  AutoFail and ElderSign hard-coded to 6 per the agenda. Skull therefore scales with Agnes's
  collected memories at the moment of the check. Bless/curse/frost (campaign bags) are valued
  via the default token-value fallback, abs applied.
- The token check (return-if-no-memory / collect-at-6+) runs after every placement (agenda
  reveal, tablet fail) and after every memory collection (a collected memory can strand tokens,
  which then return to the bag).
- Patrol: existing engine keyword used — `Patrol (LocationWithResources (atLeast 1))`.
  Engine already implements insert rules (framework 3.2, ready+unengaged, shortest path, lead
  breaks ties). Cultist token "resolve her patrol keyword" pushes `PatrolMove` directly, only
  if she is unengaged (the keyword only ever moves unengaged enemies); on Hard/Expert, if
  engaged she instead attacks each investigator engaged with her ("she makes an attack" read as
  attacking her engaged investigator(s)).
- Elder Thing token: Agnes (anywhere, if in play) picks 0–3 via amount chooser; she takes that
  much damage (source ElderThing) and the TEST investigator gets `AnySkillValue (2*n)` for the
  current test.
- "Cannot be automatically evaded": no engine modifier exists for this; implemented by
  intercepting `EnemyEvaded` on Elspeth and only letting it through when the active skill test
  is an Evade action targeting her (i.e. a real evasion). Auto-evade effects (Stunning Blow,
  Cunning Distraction, etc.) become no-ops against her, with no "evaded" windows fired.
- Elspeth's defeat flip (90023b "Triumph and Subjugation") is implemented inline on the enemy
  (Bloodless Man pattern), NOT as a separate Story entity: cancel defeat, heal all damage, show
  the back's flavor, Agnes chooses (steal 1 memory from Elspeth — only offered if she has any —
  or gain 2[per_investigator] clues), then exhaust + disengage from all. If Agnes is not in
  play (resigned), the choice is skipped, she just heals/exhausts/unengages.
- Agenda 1b "Out For Blood": Elspeth attacks each investigator in player order (regardless of
  location/exhaustion; `initiateEnemyAttack` honors CannotAttack), then `RevertAgenda` back to
  1a (doom was already removed from play by the advance).
- Act objective: forced on a `ScenarioEvent "memoryCollected"` window with criteria
  `notExists (LocationWithResources (atLeast 1))` (= all 9 collected, by either party; the
  9 total is odd so the act-back comparison can't tie). Act back routes R1 (Agnes > Elspeth)
  else R2. Act Forced: Agnes defeated -> R2 (if she was the last investigator, the engine's
  no-remaining-investigators path clears the queue and NoResolution also routes to R2 — same
  outcome per the insert).
- No resolution -> R2. XP (both resolutions): everyone gets standard XP (victory display +
  victory locations); Agnes gets max(standard, memories she collected) — implemented as a
  per-investigator bonus entry in the XP report.
- Advanced swap: R1 Agnes MAY upgrade Heirloom of Hyperborea (01012 -> 90018) OR downgrade
  advanced Dark Memory (90019 -> 01013) or do nothing; R2 she MUST upgrade Dark Memory
  (01013 -> 90019) OR downgrade advanced Heirloom (90018 -> 01012) (auto-applies if only one is
  legal, skipped if neither). Legality = the relevant version exists anywhere in the game
  (`findCard`), since the 1-of signature may be in play/hand/discard at scenario end. Mechanism:
  `RemoveCampaignCardFromDeck` + `AddCampaignCardToDeck DoNotShuffleIn`.

## Uncertainties

- The act-deck reverse-side location (random of Eztli Exhibit / Black Cave / Train Tracks /
  Arkham Police Station) is placed with `place`; these defs are single-sided so they enter play
  revealed, and none of their engine implementations have a put-into-play/revelation effect, so
  "ignoring its revelation ability" is naturally satisfied.
- Insert says "she makes an attack" (singular) for the Hard/Expert cultist; if Elspeth is
  engaged with multiple investigators we attack each engaged investigator (mirrors enemy-phase
  attack semantics). Judgment call.
- Stolen memories (90023b option 1) move from Elspeth's pool to Agnes's pool; the total
  collected count is unchanged, matching "all 9 memories have been collected (by either party)".
- Memory counts are surfaced via the skull token text, XP report, and game-log messages; there
  is no dedicated UI widget for the two pools.
