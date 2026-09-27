---
title: project_window_entry_tick_timing
description: "Cards entering play during an open window can't respond to it — enforced by gameWindowTick/gameEntryTicks in getActionsWith"
---

A card must be in play when a triggering condition occurs to respond to it; a card that enters play *during* an open window cannot react to that window's already-occurred condition (issue #4927, Hemophobia drawn mid after-damage-window).

Mechanism added: a monotonic `gameWindowTick` clock (incremented on each `CheckWindows` open in `runPreGameMessage`, Game/Runner.hs ~3719), a parallel `gameWindowTickStack :: [Int]` pushed/popped in lockstep with `gameWindowStack` (pop in `EndCheckWindow`), and `gameEntryTicks :: Map CardId Int` captured at enter-play (hooks in `runPreGameMessage` for `CardEnteredPlay`, `PlaceTreachery` entersPlay, `EnemySpawn`). Filter lives in `getActionsWith` (Helpers/Action.hs): for forced/reaction abilities only, keep iff `getCurrentWindowTick > entryTick(sourceCard)`. `getCurrentWindowTick`/`getEntryTicks` in GameEnv.hs.

**How to apply:** New entity types that enter play and have forced/reaction abilities need an entry-tick hook in `runPreGameMessage` (events that stay in play are NOT yet hooked). The filter **fails open** when there is no current window tick (`Nothing`) — so games loaded from pre-fix saves with an already-open window behave as before for that window (graceful deploy degradation), and so do replay-from-export `--undo` states whose windows predate the tick fields (can't verify suppression via plain replay — use a unit test or rewind before the damage). JSON uses `.:? … .!=` defaults so old saves load. Related: [[project_after_enter_engagement_timing]], [[project_drewcards_history_timing]], [[project_playerwindow_active_investigator_stale]].
