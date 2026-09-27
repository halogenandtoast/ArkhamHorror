---
title: project_test_useability_empty_windows
description: Test helper useAbility passes empty windows; cards re-checking in-hand ability performability crash — activate via raw UseAbility with defaultWindows
---

The `useAbility` test helper (tests/TestImport/New.hs) is `run $ UseAbility (toId i) a []` — it passes an **empty** window list. Most action-ability tests work anyway because the activated ability's own performability doesn't depend on those windows.

But a card whose handler re-checks the performability of OTHER abilities against the passed windows breaks: an in-hand `[action]` needs a `DuringYourAction`/`DuringTurn` window to read as performable, so with `[]` the re-check finds nothing and a `chooseOne` over the (now empty) list throws `No messages for chooseOne`. Hit this with True Magick: Reworking Reality (`UseCardAbility … 1 ws _` filters in-hand spell abilities by `getCanPerformAbility iid ws`).

**How to apply:** when activating such an ability in a spec, bypass `useAbility` and push real windows yourself:
`run $ UseAbility (toId self) ability (defaultWindows $ toId self)` (import `defaultWindows` from `Arkham.Window` — TestImport only re-exports `Window(..)` + a few `WindowType`s, not `defaultWindows`). The criterion that GATES the action (e.g. `HasTrueMagick`) checks with the same `getCanPerformAbility iid windows'`, so passing `defaultWindows` keeps offer-time and resolve-time in lockstep. See [[project_truemagick_signmagick_ability_exposure]].
