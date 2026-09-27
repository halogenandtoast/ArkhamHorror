---
title: project_windowask_stale_seat_reask
description: Multi-seat window AskMaps must drop stale seats on answer (WindowChooseOne marker); the trailing Do (CheckWindows ws) is what regenerates them
---

`runWindow` runs per-investigator, so a forced/reaction ability whose window is not
investigator-scoped (e.g. `EnemyEnters #when Anywhere (be a)`) is offered to EVERY
investigator at once. `WindowAsk` (Arkham/Game/Runner.hs) merges those seats into one
`AskMap` and queues `Do (CheckWindows ws)` **behind** it — that re-check is the only
thing that enforces `GroupLimit PerWindow 1` (Arkham/Helpers/Ability.hs).

So any multi-seat ask built by `asWindowChoose` must have its leftover seats **dropped**
on answer, never re-asked: a re-asked seat is stale (it enumerated choices before the
answer resolved) and resolves straight into `UseAbility` with no limit re-check.
`Entity/Answer.hs` marks these via the `WindowChooseOne` Question constructor
(`isRegeneratedWindowChoose`, alongside `PlayerWindowChooseOne`). Issue #5160:
Interstellar Traveler fired twice in 2p — 2 doom, 2 clues.

**Why a marker and not a "is a window open" heuristic:** `SkillTestAsk` AskMaps
(commit-cards) and deck selection push NO re-check, so their other seats genuinely
still need the `AskMap question'` re-ask. That re-ask exists for ChooseDeck.

**How to apply:** adding a `Question` constructor is cheap in the library but the real
blast radius is `tests/TestImport.hs` — `stripQuestionWrappers` is the choke point
(normalize new ChooseOne flavors to `ChooseOne` there and specs keep working). Watch for
`case ... of ChooseOne ... ; _ -> ...` fallthroughs, which the compiler will NOT flag:
`anyValidChoice` in Arkham/Game.hs is one (it gates asks whose TargetLabels are dead).

Related: [[project_skip_all_triggers]], [[project_window_entry_tick_timing]],
[[project_donechoosingdecks_queue_fragility]]
