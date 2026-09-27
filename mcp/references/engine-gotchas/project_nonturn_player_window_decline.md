---
title: project_nonturn_player_window_decline
description: "A non-turn investigator's fast-ability window (handlePlayerWindowV2) had no decline; it now appends SkipTriggersButton and the V2 dispatch is gated on investigatorSkippedWindow"
---

`PlayerWindow iid ...` is handled twice by the investigator runner (`Investigator/Runner.hs` ~2561):

- `iid == investigatorId` → `handlePlayerWindow` (V1, `Investigator/Runner/Action.hs` ~420) — always appends `EndTurnButton iid [ChooseEndTurn iid]`, so the turn player always has an exit.
- `iid /= investigatorId` → `handlePlayerWindowV2` (~520) — offers *other* investigators their fast abilities / fast-playable cards during the turn player's window.

**V2 historically appended nothing.** A non-turn investigator with exactly one available fast ability got a one-option, decline-less `PlayerWindowChooseOne` — their seat could only be cleared by triggering the ability or by the turn player acting. Reported as #5284 ("Ashcan" Pete forced to discard to ready Duke) and, in the stale-seat variant, #5159 (Joey the Rat). Compare `runWindow` (`Investigator/Runner.hs` ~380), which always appends `SkipTriggersButton iid` when `getAllAbilitiesSkippable` holds — V2 has no such notion because every V2 choice is optional by construction (it early-returns on `anyForced`).

Two things are needed; either alone is useless:

1. V2 appends `SkipTriggersButton investigatorId` to its choices (inside the existing `unless (null choices)` guard, so a window is never opened containing only a skip button).
2. The V2 dispatch is gated on `not investigatorSkippedWindow`.

Without (2) the skip does nothing visible: answering drops the other seats, the queue drains, and `Game.hs` ~6480 re-pushes `PlayerWindow <turnPlayer>` on the drain, so V2 rebuilds the identical question immediately — an infinite re-prompt. See [[project_windowask_stale_seat_reask]] for the seat-dropping rule this rides on.

`SkipTriggersButton iid` → `Run [SkippedWindow iid]` (`Message.hs` ~2686) → `investigatorSkippedWindow = True`, and **every `CheckWindows` clears it** (`Investigator/Runner.hs` ~2162). Because `CheckWindows ws` is processed by all entities before its paired `Do (CheckWindows ws)` (`Game/Runner.hs` ~3930), a player-window decline can never suppress a genuine reaction window. The gate is therefore a debounce, not a lockout: the seat is re-offered as soon as anything actually happens in the game.

Why the bug looked turn-order dependent: Pete's ability needs `exists (AssetControlledBy You <> #exhausted)`, and Duke only becomes exhausted during Pete's own turn — so the prompt only exists if some investigator takes a turn *after* Pete. First-in-order Pete → forced every window of the next turn; last-in-order Pete → never seen.

Other readers of `investigatorSkippedWindow`: `Ability.skipForAll` via `selectNone InvestigatorSkippedWindow` (`Helpers/Ability.hs` ~63; only user is `Agenda/Cards/TheTrueCulpritV8.hs`) and `LukeRobinson.hs` ~104. Both evaluate inside window checks, i.e. after the reset.

AI note: `Ai/Decision.hs` scores `SkipTriggersButton` as a neutral stop (5) and `uiIsStop` treats it as a stop, so AI seats can now decline these windows instead of being forced to trigger.
