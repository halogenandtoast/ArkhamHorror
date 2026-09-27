---
name: project_refillslots_ignores_at_location_assets
description: "RefillSlots rebuilt slot occupancy from AssetInPlayAreaOf/AssetInThreatAreaOf only, so a controlled asset placed AtLocation (Summoned Servitor) lost its slot on every refill"
metadata:
  type: project
---

`RefillSlots` (`Arkham/Investigator/Runner.hs` ~1907) does not add to the existing slot map —
it **empties every slot** (`allSlots' = ... emptySlot <$> ...`) and refills from
`requirements`, which came from

```haskell
select $ oneOf [AssetInPlayAreaOf (InvestigatorWithId iid), AssetInThreatAreaOf (InvestigatorWithId iid)]
```

Summoned Servitor (09080, `cdSlots = [#ally, #arcane]`) enters play and immediately places
itself `AtLocation` in its own `CardEnteredPlay`. It matches neither branch, so it was
dropped from the requirements and its slot silently freed. `InvestigatorPlayedAsset` slots it
correctly on play (it reads `field AssetSlots`, which is placement-agnostic), but
`InvestigatorClearUnusedAssetSlots` pushes a `RefillSlots` on **any** later asset play, so the
slot went away for good (#5660).

Fix: also collect `AssetControlledBy (InvestigatorWithId iid)` filtered to
`isJust . preview _AtLocation` on `AssetPlacement`. Deliberately narrow — `AttachedToLocation`
/ `AttachedToEnemy` / `InVehicle` are excluded, since nothing controlled with printed slots
uses them and widening risks spurious "discard something to make room" prompts.

**How to apply:** placement and slot occupancy are independent axes. Any code that asks "which
assets take up this investigator's slots" must key off **controller**, not off
`InPlayArea`/`InThreatArea` placement. Summoned Servitor is currently the only card that is
controlled, sits `AtLocation`, and has `cdSlots` (Brain Case has `[#ally]` but only goes to a
location while `isNothing attrs.controller`), so this seam has exactly one live user and is
easy to re-break.

Verify cheaply: inject `{"tag":"Raw","contents":{"tag":"RefillSlots","contents":["<iid>",[]]}}`
via `arkham-replay --answers` against any export — the bug reproduces on *any* refill, so no
`--undo` walk is needed.

Related: [[project_playability_slot_check_ignores_customizations]],
[[project_cdslots_silent_default_empty]], [[project_resetgame_wipes_assets_but_not_slots]].
