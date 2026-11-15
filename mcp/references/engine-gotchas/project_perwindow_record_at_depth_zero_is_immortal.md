---
name: project_perwindow_record_at_depth_zero_is_immortal
description: "A PerWindow use recorded at window depth 0 is never pruned by EndCheckWindow, and PerWindow counts windows by STRUCTURAL equality — so a recurring window shape (When FastPlayerWindow, When TurnBegins <iid>) gets blacklisted for the rest of the scenario; Safeguard (2) went dead after one arming (#5999)"
metadata:
  node_type: memory
  type: project
---

`PlayerLimit PerWindow 1` is the default limit for every reaction
(`defaultAbilityLimit`, `Arkham/Ability.hs`). Two facts about it combine badly:

1. **A record stamped `usedDepth = 0` is immortal.** `handleUseAbility`
   (`Arkham/Investigator/Runner/Action.hs`) reads `getWindowDepth` on the **first**
   use only; later uses just bump `usedTimes` and *append* to `usedAbilityWindows`. A
   use answered from a player window is not inside a `CheckWindows`, so it records
   depth 0 — and the `EndCheckWindow` purge keeps a `PerWindow` record whenever
   `depth >= usedDepth`, which at depth 0 is true forever.
2. **`PerWindow` compares window *values*, not occurrences.**
   `getCanAffordUseWith` (`Arkham/Helpers/Ability.hs`) does
   `countInWs u = length (filter (`elem` ws) (usedAbilityWindows u))`. Most windows
   carry something unique (an enemy id, a `SkillTest`), but the recurring player-window
   shapes do not: `When FastPlayerWindow`, `When TurnBegins <iid>`, `When NonFast`,
   `When DuringTurn <iid>` all have `windowBatchId = null` and
   `windowConditionTick = null`, so next turn's copy is `==` to last turn's.

So one use at depth 0 permanently blacklists that window *shape*. It bites hardest on
an ability whose matcher is turn-wide rather than a timing point — `DuringTurn` matches
every window the turn opens (#5784), so it keeps landing on exactly those recurring
shapes.

**Why:** issue #5999. Safeguard (2) (`06196`) had been armed three times across a game
of On Thin Ice, each time in a different shape, leaving

```
usedDepth: 0,  usedTimes: 3,  limit: PlayerLimit PerWindow 1
usedAbilityWindows: [When FastPlayerWindow, When TurnBegins "11017", When InitiatedSkillTest {...}]
```

With `When TurnBegins "11017"` burned, `nonBlockingExcept (TurnBegins #when Anyone)` —
the one window where it may *block* and give a deliberate prompt — could never fire
again, so the card was dead for the rest of the scenario and the only offers left rode
along in move windows, after the mover had already gone. Stripping just that record
from the export and replaying the same end-of-turn restored the prompt, which is what
pinned it.

The fix drops every seat's `PerWindow` records on `EndTurn` (`Arkham/Investigator/
Runner.hs`, beside the `PerTurn`/`PerRound`/`PerPhase` purges, via
`isPerWindowRecord`): no window spans a turn boundary, so a once-per-window bucket
cannot legitimately survive one. The depth-0 test itself is deliberately left alone —
dropping those records on any *nested* window close would let a reaction used from a
player window be re-offered inside that same still-open player window.

**How to apply:** when a reaction "works once and then never again", read the
investigator's `usedAbilities` out of the export before suspecting the matcher.
`usedDepth: 0` plus a content-free window in `usedAbilityWindows` is the signature, and
a `--undo N` replay with that record stripped is the one-line confirmation. A new
long-lived window matcher (anything turn-wide or phase-wide) should not rely on the
default `PerWindow` limit for its own dedupe.

Related: [[project_abilityisactionability_matched_play_and_resource]],
[[project_abilitylabel_without_windows_rejects_everything]].
