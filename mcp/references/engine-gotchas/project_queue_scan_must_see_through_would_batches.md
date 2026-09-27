---
title: project_queue_scan_must_see_through_would_batches
description: "A queued evade/defeat/damage step wrapped in a `Would` batch is INVISIBLE to a flat queue scan — `Would bId (x:xs)` unrolls one message at a time, so the rest stays nested; scan through `Would` and cancel the batch, don't filter the nested message"
---

`Would bId msgs` is not expanded when it is queued. The Game runner unrolls it one message at a time:

```haskell
Would _ [] -> pure $ g & currentBatchIdL .~ Nothing
Would bId (x : xs) -> pushAll [x, Would bId xs]
```

So while message `x` is running, every *later* message of the batch is still sitting inside a single `Would bId [...]` message. Any helper that scans the queue with a flat predicate (`fromQueue . any`, `popMessageMatching_`, `withQueue_ . filter`) will not see them and will silently no-op.

This bit `insteadOfEvading` (#5594). `Arkham.Behavior.Evade.pushEvadedWindows` batched the evade cascade:

```haskell
push $ Would batchId $ wouldMsgs <> [whenWindow, Do (EnemyEvaded iid eid), afterWindow]
```

Priest of Dagon's forced ability fires from `whenWindow`, at which point the queue holds `Would bId [Do (EnemyEvaded …), afterWindow]`. `beingEvaded` only matched `EnemyEvaded`/`Do`, returned `False`, and the whole "instead, ready it and place 1 doom" response was skipped — the evade resolved normally.

Two halves to the fix:

1. The predicate must recurse: `Would _ msgs -> any (isEvadedMessage attrs) msgs`.
2. The *cancel* must be `push $ CancelBatch bId`, not a filter of the nested message. Filtering would leave the after-`EnemyEvaded` window behind (reactions to an evasion that never happened) and would strand `currentBatchId` — only `Would bId []` and `CancelBatch` clear it, and `CancelBatch` explicitly does.

Do NOT reach for `cancelWindowBatch ws` from the `EnemyEvaded #when` window: `checkWindows` stamps `windowBatchId` from `getCurrentBatchId`, which for that window is whatever batch happened to be current when it was *built* — not necessarily the evade batch. Find the batch by scanning the queue for the `Would` that contains the message you mean to cancel. See [[project_cancelenemydefeat_queue_layer]] for the other way a queue filter silently no-ops.

Defeat is not batched into a `Would` this way (`Defeated`/`Do (Defeated …)` sit flat), which is why
`insteadOfDefeat` kept working while `insteadOfEvading` broke. It *is* nested when the source wrapped
its defeat checks in `simultaneously` — see [[project_cancel_helpers_must_see_through_simultaneously]].
