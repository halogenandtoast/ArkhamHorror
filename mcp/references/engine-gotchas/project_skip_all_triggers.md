---
title: project_skip_all_triggers
description: "Skip Triggers for All" is frontend-orchestrated (Game.vue); backend trusts Answer.playerId so one client skips for others; skill-test owner may skip a lone other player's fast window (issue #4897)
---

"Skip Triggers for All Investigators" (skip-all) is orchestrated entirely in the **frontend** (`frontend/src/arkham/views/Game.vue`), NOT a backend message. It sends one `Answer` per player who has a `SkipTriggersButton` pending; the backend (`library/Api/Handler/Arkham/Games/Shared.hs:233`, `fromMaybe activePlayer (answerPlayer response)`) trusts the `playerId` in the Answer payload and temporarily sets it active — so one client can legitimately submit skips for OTHER players.

Authorization: `canCurrentPlayerSkipAllWindows` — during a skill test, only the player owning `g.skillTest.investigator` may skip-all; during a turn, the active investigator's player; otherwise anyone. Intended behavior (per maintainer, issue #4897): in the fast player window between ST.1 and ST.2 (`skillTestStep = SkillTestFastWindow1`), the **test owner** should be able to skip-all to bypass other players' fast triggers — even a single lone other player. The original `skipAllAvailable` `size > 1` gate broke this when the owner wasn't being asked themselves and only one other player had a trigger (e.g. Practice Makes Perfect, a fast event committable at the owner's location).

Note: in this window the active *player* pointer can be a non-owner (the player currently being asked) while the active *investigator* is the test owner — they diverge. See [[project_playerwindow_active_investigator_stale]].
