---
title: project_reentrant_discard_laps_rescue_moves
description: A discard chain prepends to the queue, so a re-entrant Discard on an already-being-removed location laps the rescue moves a leave-play interrupt queued, and RemovedLocation defeats the investigators left behind
---

`Location/Runner.hs`'s `Discard` handler expands to a whole removal chain:

```haskell
windows [Window.WouldBeDiscarded …] <> [Discarded …] <> [RemovedFromPlay …] <> resolve (RemoveLocation …)
```

Those messages go to the **front** of the queue. A leave-play interrupt that rescues
entities off the location — Another Dimension (`02320`), `selectEach (InvestigatorAt …)
moveToEdit` — queues one `Move` per investigator, and those moves are still sitting in
the queue when the interrupt returns. If anything pushes a *second* `Discard` at the
same location before they resolve, the new chain jumps ahead of them and
`RemovedLocation` finds the un-rescued investigators still there and hands each an
`InvestigatorIsDefeated_`.

That is exactly #5388. Its trigger loop is worth recognising because it generalises:
the rescue `Move` for investigator A opens an `After Leaving A <location>` window, which
re-fires the location's *own* "after an investigator leaves" Forced (The Endless Bridge,
`02326`) — offering "discard it" a second time — while investigator B's rescue `Move` is
still queued. Any leave-play interrupt that moves entities off a location can re-enter
that location's own leave/enter triggers this way.

Two guards, both needed:

1. **Engine** — `Location/Runner.hs` now matches
   `Discard _ source target | isTarget a target && not locationBeingRemoved`, so a
   location already on its way out silently ignores a second discard (it falls through
   to the trailing `_ -> pure a`). `locationBeingRemoved` is set by the
   `When (RemoveLocation lid)` handler *before* the leave-play windows open, so it is
   reliably true for the whole interrupt. This mirrors the `not_ LocationBeingRemoved`
   guard `Arkham.Message.Lifted.Location.removeLocation` already had — the raw `Discard`
   path was the hole.
2. **Card** — The Endless Bridge's Forced is now
   `Leaves #after Anyone (be a <> not_ LocationBeingRemoved)`, so the player is not
   prompted at all. Guard 1 alone would leave "discard it" as a visible no-op *and*
   leave "place 1 doom" live — placing doom on a card that is leaving play still runs a
   doom-threshold check and can spuriously advance the agenda.

`locationBeingRemoved` is only ever set (4 sites, all immediately followed by
`RemoveLocation`) and never cleared, so guarding on it cannot strand a legitimate
discard.

Diagnosing this class: `arkham-replay --undo 1 --answers … --trace` and read the trace
in order. The tell is `RemovedLocation` / `InvestigatorIsDefeated_` appearing *earlier*
in the trace than the `Move (Movement {… moveTarget = InvestigatorTarget "<victim>"})`
that was supposed to save them.

Related: [[project_remove_from_game_skips_leaveplay]],
[[project_enemy_removal_attached_treacheries]], [[project_removed_entities_cleared]].
