# Auto-flow autopilot: the game runs itself, the player only answers real decisions

Status: draft, awaiting user approval. Target: `frontend/` (LCG). Backend untouched unless phase 6 is approved.

## Outcome

A round of play advances Mythos → Investigation → Enemy → Upkeep → Mythos without the player clicking through trivial prompts. The player is stopped only for real decisions (2+ meaningful options, commits, damage assignment with soak, hand-size discard, mulligan, story/campaign choices, deck upgrade). What the game did on its own (encounter drawn, hunter moved, enemy attacked, doom placed) is shown as a non-blocking reveal that auto-dismisses, so nothing happens invisibly.

Non-goals: porting the demo's JS engine; changing rules; a bot that plays the player's turn; touching `frontend-3ed`.

## Key finding (why this is an "answer policy", not a "phase driver")

The Haskell engine already chains phases on its own:

- `Begin MythosPhase` (`backend/arkham-api/library/Arkham/Game/Runner.hs` ~3405) queues doom → threshold → `AllDrawEncounterCard` → fast window → `EndMythos`.
- `runEnemyPhase` (`Arkham/Helpers/Phases.hs`) queues hunters move → fast window → `EnemiesAttack` → fast window → end.
- `Begin UpkeepPhase` (~3354) queues ready → draw + resource → hand size → end.

What stops the flow is every `Ask` the queue emits. Confirmed trivial ones:

| Stop | Source | Why it is trivial |
|---|---|---|
| Draw encounter card | `ForInvestigator iid AllDrawEncounterCard` builds `chooseOne [TargetLabel EncounterDeckTarget …]` even with one target | 1 click / investigator / round, no decision |
| Apply skill test results | `SkillTestApplyResultsButton` lone button (`SkillTest/Runner.hs` ~357, 464, 480, 555) | 1 click per test |
| Start skill test with nothing committable | `StartSkillTestButton` / `doneCommitting` | no decision when no card can be committed |
| Fast player windows with only Skip | `SkipTriggersButton` stops in Mythos/Enemy/Upkeep | pass by default, opt-in "stops" like MTG Arena |
| `ChooseOneAtATimeWithAuto` | auto label exists | engine already offers "auto" |
| Single valid choice | rule already exists behind a keypress in `Game.vue` ~2230-2245 | extract, don't rewrite |
| Player order in solo | `ChoosePlayerOrder` with one investigator | nothing to order |

Reference demo (`../arkham-gathering/dist`): one op queue; `window` only becomes pending when `fastOptions` exist; reactions prompt only when eligible; encounter shows a preview then resolves. We copy the *behaviour* (skip empty windows, preview what happened), not the code.

## Phases

| # | Phase | Done when | File |
|---|---|---|---|
| 1 | Ask inventory + click baseline | table of every Ask in one solo Gathering round + new-scenario start; clicks/round baseline recorded | [phase-01](phase-01-ask-inventory-baseline.md) |
| 2 | Pure answer policy | `autoAnswerPolicy.ts` classifies questions, node tests green | [phase-02](phase-02-auto-answer-policy.md) |
| 3 | Autopilot driver | `useAutopilot.ts` answers own seats safely (undo, loop, seat, double-submit guards) | [phase-03](phase-03-autopilot-driver.md) |
| 4 | "What just happened" reveal | encounter card, enemy move/attack, doom, phase banner shown non-blocking | [phase-04](phase-04-event-reveal-feed.md) |
| 5 | Settings + phase stops | Off / Standard / Full + per-phase stops, pause hotkey, i18n | [phase-05](phase-05-settings-phase-stops.md) |
| 6 | (Conditional) backend single-target encounter draw | only if phase 1-3 show the frontend cannot cover it cleanly | [phase-06](phase-06-optional-backend-trims.md) |
| 7 | Scenario transition flow | between scenarios: only story reading, campaign choices, upgrades stop | [phase-07](phase-07-scenario-transition.md) |

Dependencies: 1 → 2 → 3 → (4, 5 in parallel) → 7. 6 independent, gated on approval.

## Acceptance

- Solo Gathering round, no fast cards in hand: clicks in Mythos + Enemy + Upkeep = real decisions only (target 0 when nothing is decided), versus phase-1 baseline.
- Every auto-answered step is visible in the reveal feed and the game log.
- Undo still works: after undo the autopilot is suspended until the player answers once manually.
- Multiplayer: a client never answers another human's seat; multiplayer solitaire answers all its own seats.
- Autopilot Off = today's behaviour exactly.

## Risks

- Misclassifying a real decision as trivial → wrong play. Mitigation: allow-list (only known trivial shapes), never deny-list; Standard level conservative; reveal makes mistakes visible; undo.
- Undo/auto loop. Mitigation: suspend after undo, question signature guard, consecutive auto cap.
- Hidden info loss. Mitigation: phase 4 is mandatory, not polish.
- Backend is a fork of upstream `halogenandtoast/ArkhamHorror`; backend edits are merge debt and ripple through `backend/arkham-api/tests`. Hence frontend-first.

## Validation

`cd frontend && npm test`, `vue-tsc`, manual: full Gathering scenario solo, one 2-player game, one multiplayer-solitaire game, one scenario transition (Gathering → Midnight Masks).

## Unresolved questions

1. Default level for new players: Standard (recommended) or Full?
2. Should story/flavor screens auto-advance after N seconds (option), or always need one click?
3. Approve phase 6 (small backend change) if the frontend route proves clumsy, or stay strictly frontend?
