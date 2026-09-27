---
title: donechoosingdecks-queue-fragility
description: DoneChoosingDecks lives only in the persisted step queue behind the ChooseDeck park; loss bricks campaign start — runMessages drain branch self-heals
---

`chooseDecks` runs `Run [SetGameState IsChooseDecks, ChoosingDecks, AskMap ChooseDeck…, DoneChoosingDecks]`. The AskMap parks, so `DoneChoosingDecks` (flips state to IsActive) plus the `CampaignStep` continuation survive ONLY as the step row's persisted queue (`arkham_steps.choice->choiceMessages`). If that row's queue is ever lost (observed 2026-07-15, game "cyd": step-0 row stored `[]`; cause was a previous in-flux dev server, not reproducible on clean code), every deck/boon question still resolves but the game stays `IsChooseDecks` forever and the frontend shows an inert deck screen.

**Self-heal**: `runMessages`' queue-drained (`Nothing`) branch now re-pushes `DoneChoosingDecks` when `isChooseDecks gameGameState` and no ChooseDeck/ChooseUpgradeDeck question is parked (`isDeckQuestion` in Arkham/Game.hs). Healthy flows never drain in that state. Frontend `Campaign.vue`/`StandaloneScenario.vue` `chooseDeck` computed now keys off a parked ChooseDeck question, not `gameState == IsChooseDecks`, so a stray question isn't masked. Regression specs in [[Arkham.Game.ChooseDecksSpec]] (see also the #5151 stale-question fix: ClearUI consumes gameQuestion).

Diagnosis recipe: `arkham-replay <export> --undo N` sweep + `psql arkham-horror-backend` on `arkham_steps.choice->choiceMessages` to see the persisted queue per step.
