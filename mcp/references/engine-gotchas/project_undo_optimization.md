---
title: Undo Operation JSON Optimization
description: ArkhamGameRaw entity exists to avoid Game<->Value round-trips in the undo path
---
The undo operation was slow on the server because `stepBack`/`stepBackScenario` did 4 full `Game ↔ Value` JSON conversions per undo (fetch fromJSON + patchWithRecovery toJSON + fromJSON + replace toJSON).

**Root cause**: The game is stored as JSONB in `arkham_games.current_data`. Persistent's `PersistField Game` instance does `Value → Game` on fetch and `Game → Value` on store. The `patchWithRecovery` function additionally does `Game → Value` (to apply the patch) and `Value → Game` (to return the result).

**Fix applied**: Use `ArkhamGameRaw` (in `Entity/Arkham/GameRaw.hs`) which maps to the same table but exposes `currentData :: Value` instead of `:: Game`. Combined with `patchValueWithRecovery` and `setGameSeed` (added to `Arkham/Game/Diff.hs`), this reduces the undo from 4 conversions to 1 (`fromJSON` once for the return value).

**How to apply**: Any handler that fetches a game only to immediately serialize it back (e.g., for diffing, patching, or raw JSON operations) should use `ArkhamGameRaw` + `ArkhamGameRawKey` instead of `ArkhamGame`. Only call `fromJSON @Game` when you actually need Haskell game logic.

**Files changed**: `backend/arkham-api/library/Api/Handler/Arkham/Undo.hs`, `backend/arkham-api/library/Arkham/Game/Diff.hs`
