---
title: project_inresolution_guard_fails_open_at_endofscenario
description: handleDefeatedByModifiers's inResolution guard read False at the EndOfScenario teardown, because that same message drops the scenario out of the game mode — a permanent +3 sanity leaving play then defeated the investigator (#5918)
---

`handleDefeatedByModifiers` (`Arkham/Game.hs:7462`) pushes `CheckDefeated GameSource` whenever
an investigator's max health/sanity drops. It is guarded by `unless inResolution`, added for
#5829 (Transfiguration (2) reverting at `EndOfGame`, which with Charon's Obol killed the
investigator outright). **That guard failed open at the one message it most needed to cover.**

`ForTarget GameTarget (EndOfScenario _)` (`Arkham/Game/Runner.hs:786`) is the scenario
teardown: it wipes every non-`Persist` asset AND does `modeL .~ This c`, dropping the scenario
out of the game mode. The sweep runs on the *post*-message game, and
`getInResolution = fromMaybe False <$> scenarioFieldMaybe ScenarioInResolution`
(`Arkham/Helpers/Scenario.hs:74`) returns `False` once there is no scenario to read the field
from. So the biggest max-stat drop in the whole game — every asset leaving play at once —
was the one drop the guard did not cover.

In #5918 parallel Roland's permanent Directive `90029` ("You get +3 sanity") left play, sanity
went 8 → 5 against 5 horror, and he was "defeated by horror" after the scenario was already
over, recording 1 mental trauma on the way into the interlude. The user reported it as
**Cover Up** firing with no clues on it; Cover Up's ability 2 is correctly gated on
`CluesOnThis (atLeast 1)` and never fired at all — only the trace showed that.

Fixed by reading the field with `fromMaybe True` at that one call site: a *missing* scenario
counts as past the resolution. Correct for the whole interlude / campaign-step window, not
just the teardown message — with no scenario in the mode, nothing can be defeated.

**The bug is campaign-only and unreachable from a spec.** The mode-drop is the `These c _`
branch; standalone (`That s`) keeps the scenario, so the existing guard holds. The test harness
builds `gameMode = That scenario'` (`tests/TestImport.hs:894`), which is why the #5829
regression spec in `Transfiguration2Spec.hs` passed throughout. Verify with
`arkham-replay --undo 8 --trace` on a campaign export instead — and run the
`dist/.../build/arkham-replay/arkham-replay` path, not `stack exec`
([[feedback_verify_edited_module_in_build_log]]; the stale install copy faithfully reproduced
the pre-fix trace here too).

The same teardown message is the subject of [[project_modifier_cache_outlives_the_scenario]] — it drops the scenario from the mode,
wipes the entities, and prunes rather than rebuilds the modifier map. Both bugs come from
reading post-teardown state as if a scenario were still current.
