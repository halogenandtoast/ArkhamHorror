---
title: project_placeasset_entity_order
description: "For one message, locations run before assets (Entities.hs); an asset's own PlaceAsset handler can prepend ahead of the location's after-enter windows"
---

`Entities.runMessage` dispatches a single message to entity groups in a fixed order: acts, agendas, treacheries, events, **locations**, enemies, enemyLocations, effects, **assets**, skills, stories, investigators (`library/Arkham/Entities.hs`, instance `RunMessage Entities`). Combined with `runQueueT` prepending each entity's pushes to the front of the shared queue, this means: for one `PlaceAsset`, the **location** handler runs first (it raises `VehicleEnters #after` / Warped-Rail-style "after the X enters" windows in `Location/Runner.hs`), then the **asset** handler runs and its pushes land *ahead* of the location's already-queued windows.

**How to apply:** to make a moving asset resolve something the instant it lands — *before* that location's after-enter triggers — override `PlaceAsset aid (AtLocation newLoc) | aid == attrs.id` in the asset, call `liftRunMessage msg attrs` (sets placement so the piece is already shown at the new spot), then push the follow-up. Scope it to real moves with `case attrs.placement of AtLocation oldLoc | oldLoc /= newLoc` so setup placement doesn't trigger it. Used by the Mine Cart facing choice (Written in Rock) and `ElinaHarpersCarRunning`. Related: [[project_after_enter_engagement_timing]].
