---
name: drain-fallback-needs-scenario
description: The runMessages queue-drain fallback resumed the investigation phase between scenarios (phase is never reset) and Asked a PlayerWindow over the campaign's parked question
metadata:
  node_type: memory
  type: project
---

`gamePhase` is never reset between scenarios: `StartScenario` sets `phaseL .~ InvestigationPhase`
(Arkham/Game/Runner.hs) and `ResetGame`'s `These c _ -> This c` branch drops the scenario from
the mode while leaving the phase alone. `runMessages`' queue-drained branch (Arkham/Game.hs)
keys purely off `gamePhase`, so **any request that drains the queue while the campaign is
parked between scenarios used to resume the investigation phase** — pick a turn player, push
`PhaseStep (InvestigationPhaseStep InvestigatorTakesActionStep) [PlayerWindow …]`, and `Ask` a
`PlayerWindowChooseOne` that OVERWRITES the campaign's parked `ContinueCampaign` /
`ChooseUpgradeDeck` question. Result: no scenario + a scenario-only question, which the client
renders as a blank screen, and every later drain re-created it (`gameScenarioSteps` keeps
ticking). Fixed by `Nothing | isNothing (modeScenario (gameMode g)) -> pure ()`, ordered after
the `isChooseDecks` DoneChoosingDecks self-heal (see [[donechoosingdecks-queue-fragility]]).

Why only some requests: **an `Ask` parks and returns, so the drain branch is never reached** on
a run that ends in a question. It fires only when a run ends having asked nothing — e.g. the
`PUT /games/:id/decks` upgrade handler, whose messages ask nothing and whose parked queue is
already empty. That's how #5256 bricked: upgrade #1 ended on `Ask ContinueCampaign` (safe),
the duplicate upgrade #2 drained (clobbered).

Diagnosing this class from an export: `arkham-replay <export> --undo 1` re-runs the resume, so
the pre/post-fix difference shows up directly in `gameQuestion` + `gameTurnPlayerInvestigatorId`.
