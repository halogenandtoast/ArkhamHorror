---
name: project_resign_then_game_end_fires_both_windows
description: A forced ability on oneOf [GameEnds #when, InvestigatorEliminated #when You] fires TWICE when an investigator resigns and then ends the scenario; cap it with onlyOnce (#5650)
metadata:
  type: project
---

`taskEnds` (`Campaigns/TheDrownedCity/Helpers.hs`) and Embezzled Treasure pair
`GameEnds #when` with `InvestigatorEliminated #when You` so an eliminated
investigator's card still pays out before it leaves play. If the investigator
resigns *and then* the scenario ends, **both** windows open and the forced ability
resolves twice — a TDC Task marks 2 progress, Embezzled Treasure distributes twice.
A forced ability's default limit is `GroupLimit PerWindow 1`, which dedupes inside
one window, not across two.

**How to apply:** wrap such an ability in `onlyOnce` (= `groupLimit PerGame`). TDC
Tasks go through `taskEndsAbility a crit` in the campaign Helpers. `PerGame` records
survive elimination — `handleInvestigatorEliminated` doesn't touch `usedAbilitiesL`,
`EndCheckWindow` prunes only `PerWindow` entries, and `allInvestigators` is
`select Anyone`, which includes eliminated investigators. See also
[[project_affectsothers_drops_include_eliminated]] and
[[project_defeated_investigator_unselectable_in_elimination_window]].
