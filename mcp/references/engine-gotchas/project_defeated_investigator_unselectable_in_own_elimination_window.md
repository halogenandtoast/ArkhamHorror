---
title: project_defeated_investigator_unselectable_in_own_elimination_window
description: On the defeat path (not resign) an investigator is already flagged defeated when their own #when InvestigatorEliminated window opens, so every investigator select drops them — ControlsThis silently fails
---

`handleInvestigatorIsDefeated` (`Investigator/Runner/Damage.hs`) sets `defeatedL .~ True` in the same
handler that pushes `InvestigatorWhenEliminated`, and `Scenario/Runner.hs` opens
`checkWhen (Window.InvestigatorEliminated iid)` before `InvestigatorEliminated iid`. So during that
window `isEliminated` is already `True`, and `getInvestigatorsMatching` (`Game.hs`) drops that
investigator from **every** investigator `select` unless the matcher is wrapped in
`IncludeEliminated`. The **resign** path does not have this problem — `resignedL` is set later, in
`Do (InvestigatorResigned)` — so the same card works on resign and silently fails on defeat.

The window layer was already immune (`Helpers/Window.hs` wraps `InvestigatorEliminated` /
`InvestigatorResigned` who-matchers in `IncludeEliminated`). The criteria layer was not:
`Criteria.ControlsThis`/`OwnsThis` resolved through `select (AssetControlledBy (InvestigatorWithId
iid))` and returned `False`, so `controlled a N ... $ forced taskEnds` never fired. Every Drowned
City Task lost its progress when its owner was defeated, along with Embezzled Treasure, Unscrupulous
Loan (3) and Green Man Medallion (#5619).

**Do not fix this by moving `defeatedL`.** Three engine sites need it set before the window:
`Investigator/Runner.hs`'s `playableCards` suppression (the only thing stopping a defeated
investigator playing cards during their own elimination window), `handleInvestigatorKilled`'s
`unless investigatorDefeated` re-entry guard (`InvestigatorKilled` is queued *before*
`InvestigatorWhenEliminated`, so deferring the flag loops `KilledIfDefeated` forever), and the
`defeated || resigned` short-circuits in direct-damage/assign-damage.

**How to apply:** `ControlsThis`/`OwnsThis` now use `(InvestigatorWithId iid).includeEliminated`, so
controlled-source criteria work in elimination windows. Anything *else* that resolves an investigator
during an elimination window still needs the wrap explicitly — a plain `You`,
`InvestigatorWithId iid`, `locationWithInvestigator iid` or `assetControlledBy iid` resolves to
nothing there. Two live consequences of that: `InvestigatorsFieldCalculation You InvestigatorHealth`
sums to **0** for an eliminated investigator (so a `damage - health >= 0` threshold becomes trivially
true — see `ToeTheLine`/`DreamsOfDestruction`, which now pass `IncludeEliminated You`), and
Helios Telescope / Staff of the Serpent / Dimensional Beam Machine still hand off nothing because
their bodies call `locationWithInvestigator controller`. Note `Anyone` is on the `includeEliminated`
whitelist, so `getPlayerCount`/`PerPlayer` thresholds are unaffected.

Beware tests that fake the window with `run $ CheckWindows [mkWhen (Window.InvestigatorEliminated
iid)]` — they never set `defeatedL`, so they pass while the real defeat path fails. Drive the real
message (`InvestigatorIsDefeated`) instead.
