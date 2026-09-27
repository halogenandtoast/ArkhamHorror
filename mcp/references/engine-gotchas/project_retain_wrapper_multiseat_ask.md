---
title: project_retain_wrapper_multiseat_ask
description: "ClearUI wipes the WHOLE published question map on every answer, so any multi-seat AskMap is either self-rebuilding, barriered, or Retain-wrapped — there is no fourth option; the after-skill-test AskMap was none of them and silently destroyed the other seats' baked messages (#4787)"
---

**Every accepted answer pushes `ClearUI`, and `ClearUI -> questionL .~ mempty` (Game/Runner.hs) blows away the entire `gameQuestion` map — not just the answering seat's entry.** Seats still owed a question have to come back from somewhere. There are exactly three sources, and a multi-seat ask that uses none of them loses its other seats along with whatever messages they were holding:

1. **Self-rebuilding** — the queue re-poses it. `PlayerWindow` re-pushes itself; `WindowAsk` queues a trailing `Do (CheckWindows ws)`; the skill-test loop re-asks the commit window. `Entity/Answer.hs` deliberately drops these on answer (`isRegeneratedWindowChoose`): a re-parked window seat enumerated its choices *before* the answer resolved, so re-asking it resolves straight into `UseAbility` with no limit re-check (#5160, #5159, #5164).
2. **Barriered** — `BeginSimultaneousAsk` holds durable per-seat slots in `gameSimultaneousAsks` and republishes them on `SeatResolved`. Hard constraint (`SimultaneousAsk.hs`): **a seat's sub-flow must run to completion without parking**, because the queue is global and answering any question drains all of it (#5173). Deck selection is the only user.
3. **`Retain`-wrapped** — `Retain Message` (Arkham/Message.hs, beside `Priority`) wraps the `Ask`/`AskMap`. `Arkham/Game.hs`'s `go'` carries the flag down to whichever ask the message publishes and stores it as `gameRetainedQuestion`; `Entity/Answer.hs` then re-parks every other seat as `Retain (AskMap …)` instead of dropping them. Use this when the seats hold **baked message lists** rather than a re-enumerable set of choices — nothing about them goes stale, so re-parking is safe.

**#4787 was the missing case.** `AfterSkillTestOption` (Game/Runner.hs) `popMessagesMatching`-drains every pending after-skill-test message and republishes them as one cross-player `AskMap`. Nothing is left in the queue behind it, so it is neither (1) nor (2). One player answering destroyed every other player's option: Unrelenting (1)'s three `UnsealChaosToken` messages went with it and `Zero`/`PlusOne`/`Zero` left the chaos bag permanently. (The token symptom has been masked since `5ae6331d38` added an unseal sweep to `SkillTestEnds`, but the option-dropping cause survived until `Retain`.)

Two details that decide whether a `Retain` re-park actually works:

- **The re-park must itself be `Retain (AskMap …)`.** Otherwise retention is lost after the first answer and the second-to-last seat is dropped instead of the last.
- **Fold the answering seat's own re-ask into the same map.** `go` returns `[uiToRun m', Ask playerId (ChooseOneAtATime rest)]`; emitting that `Ask` separately parks it *ahead* of the other seats, serialising a question whose whole point is that the table picks the order.

**Known limit:** if the answering seat's payload parks (Quick Thinking pushes a `PlayerWindow`), the other seats sit behind that sub-flow in the queue. They are no longer lost, but the table waits. A single global queue can't do better without per-seat queues — the same wall `SimultaneousAsk` documents.

## The queue normalizer that comes with it

`Retain` is a transport wrapper, so a naive `\case Ask{} -> True` predicate stops matching the moment a message is wrapped — silently, or as a crash for `insertAfterMatching` (which `error`s when it finds no anchor). Rather than teach ~150 predicates about it, `Arkham/Classes/HasQueue.hs` has a `QueueWrapper` class with `stripQueueWrappers`, and every predicate-taking primitive (`findFromQueue`, `popMessageMatching(_)`, `popMessagesMatching`, `removeAllMessagesMatching(M)`, `replaceMessage(Matching)(M)`, `replaceAllMessagesMatching`, `pushAfter`, `insertAfterMatching(OrNow)`, `assertQueue`, plus `insertAfterMatchingMaybe` in Message/Lifted.hs) matches through it.

Policy is **normalize on the way in, strip on the way out**: matchers see the stripped message, and anything handing the matched message back (pop/find/replace) hands back the stripped form. That is what keeps the ~30 replacers shaped `\case Do (…) -> …; _ -> error "invalid match"` working — they never see a wrapper.

**The instance is deliberately only `Priority` and `Retain`.** Do not widen it. Roughly 40 predicates treat `Do x` and `x` as genuinely different messages (`Enemy/Helpers.cancelEnemyDefeat`, `cancelEndTurn`, `Helpers/Window.replaceWindow`, …), and unwrapping `MoveWithSkillTest` is load-bearing in `handleSkillTestNesting`. Adding a wrapper there silently rewrites the semantics of every call site.

Whole-list callbacks (`withQueue`, `withQueue_`, `fromQueue`, `mapQueue`, `overMessagesM`) cannot be normalized generically — they take `[msg] -> r`, not a predicate. Fix those by hand when they need to see through a wrapper (`Arkham/Game.hs`'s `updateChooseDeck` is one, kept in step with the `findFromQueue` directly above it).

**How to apply:** publishing a multi-seat `AskMap` whose seats hold pre-built message lists? Wrap it in `Retain`. Publishing one whose choices are recomputed from game state? Leave it unwrapped and make sure the queue re-poses it. Related: [[project_windowask_stale_seat_reask]], [[project_donechoosingdecks_queue_fragility]], [[project_skip_all_triggers]], [[project_afterskilltest_missing_ability_anchor]], [[project_setglobal_priority_victory_clearqueue]].
