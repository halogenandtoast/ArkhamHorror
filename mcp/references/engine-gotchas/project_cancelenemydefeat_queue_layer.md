---
title: project_cancelenemydefeat_queue_layer
description: "Inside runQueueT, use the LIFTED cancelEnemyDefeat (Arkham.Message.Lifted), not Arkham.Enemy.Helpers — the non-lifted one filters the local inbox, not the real queue"
---

`runQueueT` allocates a *fresh local inbox* IORef; inside the QueueT body `messageQueue = ask` returns that local inbox, and `pushAll` flushes it to the parent queue only when the block ends.

So queue-*mutating* helpers that call `withQueue_`/`popMessageMatching`/`filterInbox` directly (e.g. `Arkham.Enemy.Helpers.cancelEnemyDefeat`) only touch the local inbox when run inside a `runQueueT` block — they CANNOT remove a pre-existing message (like a queued `Defeated`/`DefeatMessage`) that lives in the global `GameT` queue. The filter silently no-ops.

To mutate the real queue from inside `runQueueT`, use the LIFTED variant from `Arkham.Message.Lifted` (`cancelEnemyDefeat = lift $ Msg.cancelEnemyDefeat`), which escapes the QueueT layer down to `GameT`. Working cards (TheBloodlessMan, CatsOfUlthar) call the lifted one directly.

Constraint pattern for a helper that wraps a lifted queue op (mirror `cancelChaosToken`):
`(ReverseQueue (t m), HasQueue Message m, MonadTrans t, Sourceable source) => ... -> t m ()`
Do NOT use `ReverseQueue m => ... -> m ()` if you need the lifted op — `m` can't be decomposed into `t m'`, and `ReverseQueue GameT` has no instance.

Bug this fixed: By the Book's `healCultistInsteadOfDefeat` ([[project_drewcards_history_timing]] area) used the non-lifted cancel, so cultists "would be defeated → heal instead" still went to discard/victory display.
