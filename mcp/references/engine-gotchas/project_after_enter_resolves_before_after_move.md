---
title: project_after_enter_resolves_before_after_move
description: "Enters #after resolves strictly BEFORE Moves #after on the same move, so two Forced abilities that both read \"after you move\" are orderable by picking the window"
---

Both windows fire on one move, in a fixed order: **`Enters #after` first, `Moves #after` second.** Two cards whose printed text is equally "after you move to/into a location" are therefore orderable by which window they take — no priority mechanism needed.

Why, in `handleDoResolveMovement` (`Investigator/Runner/Movement.hs`): `postEnterMsgs` is `[runWhenMoves, runAtIfMoves, ResolveMovement iid, runAfterMoves, ResolvedMovement …]`, so `runAfterMoves` sits *behind* `ResolveMovement`. `Do (ResolveMovement)` then pushes, in `runQueueT` written order, `WhenWillEnterLocation` → `Simultaneously [… EnterLocation …]` (which is where `RevealLocation` comes from) → `After (MoveTo)` → `moveAfter` → `MovedButBeforeEnemyEngagement` → `CheckEnemyEngagement` → `afterEntering`. That whole batch prepends in front of the queue's remaining `runAfterMoves`.

Consequences:

- The destination is **already revealed** at `Enters #after` (`EnterLocation` pushes `RevealLocation` earlier in the same batch), so an "after you move" effect that reads revealed/unrevealed state sees the arrival as revealed in either window.
- `Enters #after` also fires for a `Place` (`afterEntering` is pushed unconditionally; only `MovedButBeforeEnemyEngagement` is gated on `movement.means /= Place`). `Moves #after` does not. Pick `Moves` when the card must mean a move and not a placement.
- `MovedButBeforeEnemyEngagement #after` is a third slot between them, but it is Track Shoes' printed timing — do not borrow it just for ordering.

Night of Vespers / The End of Seasons is the case that forced this out. Act 2a *A Family Affair* prints "**Forced** - After you move to a new location: Set each unrevealed location in play aside" and every hotel location prints "**Forced** - After you move into X, put each of the following locations into play, unrevealed". With the act on `Moves #after` and the locations on `Enters #after`, the locations added their neighbours first and the act immediately swept them: the board dead-ended with nothing to move to. Swapping the two windows — act on `Enters #after You Anywhere`, locations on `Moves #after You AnySource Anywhere (be a)` — resolves the removal first, and both readings stay faithful to the printed text.

Second trap from the same drop: an act that needs a **bare** `getAbilities` list (to drop the attrs' clue-spend ability 999, see `actAbilities'` in `Act/Import/Lifted.hs`) also loses the attrs' `guard (onSide A attrs)`. A Family Affair's own advance body moves every investigator to Trattoria del Parche, which would re-fire its own Forced while it sat on side B. Write `guard (onSide A a) *> [ … ]` whenever you replace the list rather than extend it.
