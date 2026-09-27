---
title: project_enemylocation_discovery_gap
description: Enemy-locations are matched by location matchers but NOT processed by the Location runner; message handlers must be mirrored
---

Enemy-locations (`enemyLocationsL`, e.g. Hemlock House rooms) are surfaced to **location matchers/queries** as proxies (`getLocationsMatching` adds `enemyProxies`; `maybeLocation` falls back to `toEnemyLocationProxy`), so `select`, `field LocationClues`, `LocationWithInvestigator`, etc. all work for them.

BUT the `Location` `RunMessage` instance never runs for them (they aren't `Location` entities), so any message handler defined only in `Arkham/Location/Runner.hs` is silently absent for enemy-locations. This caused issue #4862: investigating a Living Bedroom that held clues discovered nothing, because `Successful (Action.Investigate)` → `DiscoverClues | DiscoverAtLocation` lived only in the Location runner.

**Why:** the EnemyLocation runner pushed `Successful (Investigate, …)` expecting downstream handling that didn't exist for it.

**How to apply:** the shared discovery logic now lives in `Arkham.Helpers.Discover` (`resolveSuccessfulInvestigation`, `resolveDiscoverCluesAt`), called from BOTH `Location/Runner.hs` and `EnemyLocation/Runner.hs`. When adding location behavior that enemy-locations should share, extract it to a helper and invoke from both runners rather than assuming the Location runner covers them.

Issue #4862 needed THREE fixes for enemy-location clue discovery:
1. Shared `Successful (Investigate)`/`DiscoverClues` handlers (above) — so discovery resolves at all.
2. `project @Location` (Game.hs) now uses `maybeLocation` (not a bare `locationsL` lookup) so `fieldMay`/`project` resolve enemy-locations like `field`/`getLocation` do. Previously `locationMatches` (which uses `fieldMay LocationClues/Doom/Horror/Shroud/Resources`) treated enemy-locations as empty, breaking `OnLocation`/`AbleToDiscoverCluesAt` criteria (Lucius Galloway's reaction didn't fire).
3. `EnemyLocation/Runner.hs` now handles `MoveTokens` (mirror of Location runner): discovery moves clues via `MoveTokens fromLocation toInvestigator`; without the source-removal half the clue was added to the investigator but never removed from the enemy-location.

General rule: `field`/`getLocation`/`select`/enemy matchers include enemy-location proxies, but `project`/`fieldMay` and the Location *runner*'s message handlers historically did not — audit both when touching enemy-location behavior. Related: [[project_drowned_city]].
