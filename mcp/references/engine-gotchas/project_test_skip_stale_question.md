---
title: project_test_skip_stale_question
description: "Test helpers answer questions without the ClearUI the API pushes ahead of every answer, so declining a triggers window leaves the answered question in gameQuestion — `skip; assertNoReaction` silently always fails"
---

The real answer path (`Api/Handler/Arkham/Games/Shared.hs` ~401) enqueues **`ClearUI` ahead of the answer's messages**:

```haskell
queueRef <- newQueue ((ClearUI : syncMsgs <> messages) <> currentQueue)
```

`ClearUI` is what wipes `gameQuestion` (`Game/Runner.hs` ~184). The test harness's `chooseOptionMatching` / `chooseOnlyOption` / `skip` (TestImport.hs) just do `push (uiToRun msg) <* runMessages` — **no `ClearUI`**. That is invisible for a normal answer, because the resulting cascade pushes a new `Ask` that overwrites `gameQuestion`. It is NOT invisible for an answer that produces no new question — which is exactly what declining a triggers window does:

`WindowAsk` pushes `[Ask pid q, Do (CheckWindows ws)]` (`Game/Runner.hs` ~1770). Answering with `SkipTriggersButton` runs `SkippedWindow iid`, which sets `investigatorSkippedWindow` and gates that trailing `Do (CheckWindows ws)` (`Investigator/Runner.hs` ~2183). Correct behavior — but the *answered* question is still sitting in `gameQuestion`, so `assertNoReaction` re-reads it and reports the reaction you just declined.

This is a harness artifact, not an engine bug: reproduce it with any plain in-play [Spell] `[action]` + Sign Magick (3) — nothing to do with the card under test.

**How to apply:** when a spec needs "declining this window ends it", mirror the server by pushing `ClearUI` *before* the decline:

```haskell
skipWindow :: HasCallStack => TestAppT ()
skipWindow = push ClearUI >> skip
```

(`push` does not run messages, so `skip` still sees the live question, and its answer lands in front of the `ClearUI`.) Same trap applies to any `chooseOptionMatching` whose branch creates no follow-up question before an assertion that reads `gameQuestion`. Related: [[project_damage_question_wrappers]], [[project_test_useability_empty_windows]], [[project_nonturn_player_window_decline]].
