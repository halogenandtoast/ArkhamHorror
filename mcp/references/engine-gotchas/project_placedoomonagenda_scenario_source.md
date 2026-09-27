---
title: project_placedoomonagenda_scenario_source
description: "placeDoomOnAgenda uses the scenario as source, so SourceIsCardEffect doom-redirect windows never fire for it"
---

`placeDoomOnAgenda`/`placeDoomOnAgendaAndCheckAdvance` (Lifted) push `PlaceDoomOnAgenda`, which the Scenario runner turns into `PlaceTokens (toSource scenario) (AgendaTarget) Doom n`. The agenda's `PlaceDoom source (isTarget a) n` handler still runs `wouldDo` and generates a `WouldPlaceDoom` window, BUT the source is the **scenario** (`ScenarioSource -> False` in `SourceIsCardEffect`).

So a card effect that places doom on the agenda via `placeDoomOnAgenda` will NOT trigger forced abilities keyed on `WouldPlaceDoomCounter ... SourceIsCardEffect` (e.g. The Onslaught redirecting doom to The Captives `[[project_drowned_city]]`-adjacent Feast content).

**How to apply:** For a *card effect* (event/asset/enemy/location/treachery) that places doom on the current agenda, use the source-carrying helpers in `Arkham.Message.Lifted`: `placeDoomOnAgendaBy <cardSource> n` / `placeDoomOnAgendaAndCheckAdvanceBy <cardSource> n` (instead of `placeDoomOnAgenda n` / `placeDoomOnAgendaAndCheckAdvance n`). These select `AnyAgenda` and `placeDoom <source>` so the `WouldPlaceDoom` window carries the card's source. Net doom effect is identical in normal scenarios.

Fixed in all three *player* cards that place agenda doom: `FickleFortune3`, `DarkMemory` (scripted `onPlay`, pass bare `source`), `DarkMemoryAdvanced` (old-style, inlined `PlaceDoom (toSource attrs) ...`). The same latent bug still exists in ~38 encounter-layer call sites (treachery/enemy/location/act/agenda) — but NONE of those cards are in The Longest Night's encounter sets, so they don't affect The Onslaught today; they'd matter only for a future `SourceIsCardEffect` doom-redirect ability. Scenario/mythos-phase `placeDoomOnAgenda` should stay scenario-sourced (correctly NOT a card effect).
