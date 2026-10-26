---
name: project_choose_turn_player_is_the_one_turn_order_decision
description: Whose turn is next in the investigation phase is now ChooseTurnPlayer alone; the phase used to pre-build the first turn's question before its own begins-window had run, so nothing in that window could affect turn order
metadata:
  node_type: memory
  type: project
---

Two places decided whose turn came next in the investigation phase, and they
disagreed about *when*:

- `Begin InvestigationPhase` (`Game/Runner.hs`) built the first turn's
  `ChooseOne` eagerly, out of `getInvestigators`, and queued it inside
  `PhaseStep (InvestigationPhaseStep NextInvestigatorsTurnBeginsStep)` — i.e.
  **before** the `PhaseBegins #when #investigation` window it queued ahead of
  it had run. A card in that window could not influence the first turn at all,
  and it did not filter by `endedTurn`/`resigned`/`defeated`.
- the `runMessages` queue-drain fallback (`Game.hs`) chose every turn after
  that, from `gamePlayerOrder` filtered by "already done this round".

Both now push `Message.ChooseTurnPlayer`, handled once in `Game/Runner.hs`:
filter `gamePlayerOrder` by `Helpers.Investigator.doneWithTurn`, narrow to the
seats carrying `Modifier.ResumesSuspendedTurn` if any of them still has a turn
coming, then `[] -> pushEnd EndInvestigation` / `[x] -> ChoosePlayer x
SetTurnPlayer` / otherwise ask the lead. The eager question is gone, so the
phase's own begins-window is the last word on who goes first.

Why it had to be deferred, not just filtered: /Ritual Circle: A Harnessed
Future/ (Ages Unwound `:ages-unwound:224`) returns the seats it suspended on a
`SilentForcedAbility (PhaseBegins #when #investigation)` and flags them
`ResumesSuspendedTurn`, so that the turn it suspended resumes "at the start of the
next investigation phase". Several flagged seats are still *chosen between*
rather than ordered for the players — the modifier narrows the offer, it does
not sequence it.

Notes for anything that touches this:

- The drain fallback is reached only on a run that ends having asked nothing
  (see [[drain-fallback-needs-scenario]]), which is why the turn-player decision
  can live behind a message rather than being computed inline.
- `ResumesSuspendedTurn` on a seat that never gets a turn (defeated before it) is
  harmless: `doneWithTurn` drops it, and an empty priority set falls back to
  everyone.
- `error "no investigators"` was dropped with the eager branch; an empty
  investigation phase now ends instead of throwing, matching what the drain
  fallback already did.
- A resumed turn is not a turn that *begins*. The same choice resolves as
  `Question.ResumeTurnPlayer`, which applies `turnStartState` (the bookkeeping
  `BeginTurn` does, undo boundary included) and pushes no `TurnBegins` windows;
  the queue drain then opens the action window, so the phase lands in 2.2.1.
  `Begin InvestigationPhase` also leaves out its own player window between 2.1
  and 2.2 when any seat carries the modifier — a turn that resumes as the phase
  begins is already in progress, so the phase never reaches a point between
  turns. That read happens in `Begin InvestigationPhase`, before the
  `PhaseBegins` window, which is why the modifier has to be on the seat while it
  is still suspended and not only once it is handed back.
