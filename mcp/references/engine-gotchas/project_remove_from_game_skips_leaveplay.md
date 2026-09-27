---
title: project_remove_from_game_skips_leaveplay
description: Bare RemoveEnemy/Remove fires NO leave-play window; encounter-set purges must go through RemoveFromPlay, and attached events now self-discard on RemovedFromPlay
---

`RemoveEnemy eid` is a pattern synonym for `Remove (EnemyTarget eid)`. Its `Game/Runner.hs`
handler only flips the placement to `OutOfPlay RemovedZone` — **no `LeavePlay` window, no
`Discard` broadcast, no `RemovedFromPlay`**. Only `When (RemoveEnemy …)` fires the `#when`
window, and that wrapper exists solely when someone pushes `resolve (RemoveEnemy …)`.

So a bare `push $ RemoveEnemy (toId a)` silently strands everything attached to the enemy.
`RemoveAllCopiesOf{,Encounter}CardFromGame` in `Enemy/Runner.hs` did exactly that, which is
how Rod of Carnamagos lost its Rot when *Kingdom of the Skai* purged the Zoogs set on advance
(#5309). Fixed with a `removeFromGameMessage` helper: `RemoveFromPlay (toSource a)` when
`isInPlayPlacement a.placement`, bare `RemoveEnemy` otherwise (victory display / removed zone).
`RemoveFromGame` at `Enemy/Runner.hs` already did the right thing — copy that, not the bare push.

Removing from the game *is* leaving play, so every leave-play Forced must still fire. When
writing or reviewing code that takes an entity out of the game, push
`RemoveFromPlay (toSource a)` (or `resolve …`), never a bare `Remove*`. `Enemy/Runner.hs`'s
`PlaceUnderneath` clause still has the bare-push shape and would drop attachments the same way.

Companion fix: events were the only attachable entity with no `RemovedFromPlay` clean-up —
`Treachery/Runner.hs` and `Enemy/Runner.hs` both self-discard when their attached host leaves
play, `Event/Runner.hs` only handled the `Discard` path. It now has the mirror clause, guarded
with `not (isSource a source)` so the event's own `RemoveFromPlay` doesn't re-enter. Ordering is
safe: the Forced's `PlaceInBonded` resolves inside the leave-play window and
`When (PlaceInBonded …)` deletes the event entity before the host's `RemovedFromPlay` lands, so
no double handling.

Diagnosing this class of bug: the tell is an entity left in `gameEntities` with a `placement`
pointing at an id that no longer exists. `arkham-replay --undo N --answers` with a
`{"tag":"Raw", …}` message reproducing the removal, plus `--trace`, shows the whole removal as
just two lines when the windows are being skipped.

Related: [[project_enemy_removal_attached_treacheries]], [[project_removed_entities_cleared]].
