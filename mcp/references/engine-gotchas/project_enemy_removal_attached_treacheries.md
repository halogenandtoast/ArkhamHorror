---
title: project_enemy_removal_attached_treacheries
description: Treacheries attached to an entity self-discard on its RemovedFromPlay (engine-wide); enemy removal only needs to discard attached ASSETS explicitly
---

When an entity leaves play it pushes `RemovedFromPlay <source>`. Every treachery sees
this and, in `Treachery/Runner.hs` (~line 166), checks `a.attached` against the source —
if it's attached to the thing being removed, it discards itself. So treacheries attached
to an enemy (e.g. Sudden Mutation `c10741` on Cavern Moss) are cleaned up **engine-wide**
automatically when the host enemy is removed. A card that removes/discards a host enemy
does NOT need to discard its attached treacheries itself.

Attached **enemies** (Cavern Moss) and, since #5309, attached **events** (Rod of Carnamagos'
Rots) have the same self-discard clause in their runners. Assets are the exception: they have
no such self-cleanup, so `Enemy/Runner.hs` `RemovedFromPlay` explicitly discards
`EnemyAsset enemyId`. Don't mirror that for treacheries/events — it would double-handle.

None of this fires if the host is taken out via a bare `Remove*` message, which skips
`RemovedFromPlay` entirely — see [[project_remove_from_game_skips_leaveplay]].

Example: Cavern Moss (`c10585`), when its attached Item asset leaves play, simply
`toDiscard GameSource attrs` (discards itself); the engine discards the attached Sudden
Mutations. `Arkham/Enemy/Cards/CavernMoss.hs` (issue #4891).

Related: a dangling `AttachedToAsset` placement used to crash `onSameLocation`
(`Helpers/Placement.hs`); now uses `fieldMay`.
