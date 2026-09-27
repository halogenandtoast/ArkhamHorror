---
name: project_cancel_helpers_must_see_through_simultaneously
description: "`simultaneously` hides the defeat cascade two groups deep, so a flat `withQueue_ . filter` cancel silently no-ops — use `removeAllMessagesMatchingNested`"
metadata:
  node_type: memory
  type: project
  originSessionId: 16dfa548-d4f3-4b4a-9b20-6afbbf372602
  modified: 2026-09-25T22:29:35.559Z
---

`Arkham.Message.Lifted.simultaneously` pushes `Run [Simultaneously msgs]`, and
`interleaveSimultaneously` re-splices each branch's captured output as
`Simultaneously [Run …, Run …]`. So a defeat cascade built inside a `simultaneously` block sits
**two groups deep** while its own `EnemyWouldBeDefeated #when` window is open — invisible to any
`withQueue_ $ filter …` / `fromQueue . any` scan.

Storm of Spirits (`simultaneously $ for_ eids (checkDefeated attrs)`) vs The Thing That Follows:
`cancelEnemyDefeat` no-op'd, the enemy was still shuffled into its bearer's deck, and the
surviving nested `DefeatMessage` reached `runGameMessage`'s history case (`Game/Runner.hs`),
which does `getEnemy eid` → `Unknown enemy: <eid>` 500. Only Storm of Spirits reproduced it
because every other kill path pushes `[whenMsg, afterMsg, Defeated …]` flat; Storm of Spirits (3)
uses a plain `for_` and was fine.

**How to apply:** any queue-mutating cancel must use the `*Nested` primitives in
`Arkham/Classes/HasQueue.hs` — `removeAllMessagesMatchingNested` (added for this),
`popMessagesMatchingNested`, `wrapMessagesMatchingNested`. They descend via `queueGroup`, whose
`Message` instance covers `Simultaneously` and `Run`. All three `cancelEnemyDefeat*` helpers in
`Arkham/Enemy/Helpers.hs` now do. This corrects the closing line of
[[project_queue_scan_must_see_through_would_batches]] ("defeat is not batched this way") —
defeat *is* batched whenever the source used `simultaneously`.

Related: [[project_cancelenemydefeat_queue_layer]] (use the *lifted* cancel inside `runQueueT`),
[[project_simultaneously_isolates_the_queue]].
