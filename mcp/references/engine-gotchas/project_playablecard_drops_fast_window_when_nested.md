---
name: project_playablecard_drops_fast_window_when_nested
description: "PlayableCard's stacked-window branch injected only duringTurnWindow, dropping FastPlayerWindow, so every fast card looked unplayable inside a nested window on your turn (Double, Double vs Cheat the System, #5410)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 33b93cfb-87d3-4bb5-8d54-8264d2c0d032
  modified: 2026-08-16T00:48:33.662Z
---

The during-turn player window (`PlayerWindow`) is **not** a `CheckWindows`, so
`gameWindowStack` is empty while a card is being played from it. The first nested
`CheckWindows` (e.g. `PlayCard #after`) makes the stack non-empty — and
`Arkham/Game.hs`'s `PlayableCard` branch then took its non-`Nothing` path, which
re-injected only `Window.duringTurnWindow tiid`. The `Nothing` fallback right above uses
`Window.defaultWindows` = `DuringTurn` + `NonFast` + **`FastPlayerWindow`**, so
`FastPlayerWindow` silently vanished the moment any window opened.

`Helpers/Playable.hs` gates a fast card on `inFast` alone (`noAction` is `False` whenever
`cdFastWindow` is `Just`, and the `isNothing cdFastWindow && notFastWindow` disjunct
can't help), so every generically-fast card reported unplayable inside a nested window.
Double, Double (05320) triggers on `PlayCard #after You $ PlayableCard (UnpaidCost
NoAction) …`, so it never offered to replay Cheat the System (1) or any other fast event
played on your own turn. Off-turn/mythos plays were fine — those come from a real
`checkWindows [mkWhen FastPlayerWindow]`, which stays on the stack.

**How to apply:** the stacked branch now injects
`[duringTurnWindow tiid, mkWhen FastPlayerWindow]`. Blast radius is only cards with
`cdFastWindow = Just …` — for everything else `noAction`/`notFastWindow` already passed.
Separately, "play that card" effects that pass their own window list
(`playCardPayingCostWithWindows`) must add the fast window too, because
`Game/Runner.hs`'s `PlayCard` re-validates with `getIsPlayable … PaidCost windows'`;
`playCardPayingCost` (plain) is safe since `payCardCost` uses `defaultWindows`. Related:
[[project_skipplaywindows_swallowed_after_window]],
[[project_during_your_action_window]], [[project_playerwindow_active_investigator_stale]].
