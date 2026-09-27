---
name: project_yourlocation_window_handler_fanout
description: "You"-scoped windows already fire once per investigator; a handler that also selectEaches over investigators deals N² effects
metadata:
  type: project
---

`EnemyEntersYourLocation` (and every other `You`/`YourLocation`-scoped window) is
pushed by the engine as **one separate `CheckWindows` per investigator** —
`Arkham/Enemy/Runner.hs` builds `[(iid', eid') | iid' <- iidsHere, eid' <- entries]`
and `Matcher.EnemyEntersYourLocation` only matches when `iid == iid'`
(`Arkham/Helpers/Window.hs`). So a forced ability on such a window is already
correctly scoped: it triggers once per investigator, and `UseThisAbility`'s `iid`
is that investigator.

A handler that *also* fans out (`selectEach (InvestigatorAt ...) \iid -> ...`)
therefore deals **N² effects** for N investigators at the location — and the player
sees the prompt N times. Altered Beast (02096) did exactly this: two investigators
at the Brood of Yog-Sothoth's destination got 4 horror and 4 prompts instead of 2
(#5344).

**Why:** the per-investigator scoping lives in the window layer, not the handler,
so it's invisible when you read the handler alone.

**How to apply:** if the ability's window matcher says `You`/`YourLocation`, the
handler must act on the `UseThisAbility` `iid` only — never re-select the
investigators at the location. `Pursued.hs` and `AllosaurusRampagingPredator.hs`
are the reference idiom. Conversely, if a card really is "each investigator there
does X once", use a non-`You` window (`EnemyEnters`, `Enters` with a location
matcher) so it fires once, then fan out in the handler.

Related: [[project_affectsothers_active_investigator]],
[[feedback_engaged_enemy_movement_ruling]],
[[project_after_enter_engagement_timing]]
