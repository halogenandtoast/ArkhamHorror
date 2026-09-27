---
title: project_during_your_action_window
description: "DuringYourAction vs DuringTurn window split — action abilities use DuringYourAction (matches NonFast, works in granted actions); \"play during your turn\" fast cards stay DuringTurn (genuine turn only)"
---

The engine distinguishes two window matchers that used to be conflated (#4894):

- **`Matcher.DuringYourAction Who`** = "you have an action to take." Matches the
  `Window.NonFast` action-taking window (present on your real turn AND during a granted
  "as if it were your turn" action), plus `DuringTurn`/`FastPlayerWindow`.
- **`Matcher.DuringTurn Who`** = "it is genuinely your turn." Matches ONLY a real
  `Window.DuringTurn` window (+ `FastPlayerWindow` via `TurnInvestigator`). Does **not**
  match `NonFast`.

Key consequences:
- `defaultAbilityWindow` for `ActionAbility`/`ServitorAbility` is `DuringYourAction You`
  (`Ability.hs`), so basic/action abilities remain usable with a granted action.
- "Play during your turn" Fast cards use `cdFastWindow = Just (DuringTurn You)` and now
  require a genuine turn — they are NOT offered with a granted action (the #4894 fix).
- Granted/immediate player windows (`takeActionAsIfTurn` → `handlePlayerWindow immediate=True`)
  present `Window.NonFast` ONLY — no fabricated `DuringTurn`, no `FastPlayerWindow`. The
  `AsIfTurn` modifier is now vestigial (set by `takeActionAsIfTurn`, no longer read).
- The `DuringTurn` **criterion** (Criteria.hs, distinct from the window matcher) is unchanged:
  evaluated as `selectAny (TurnInvestigator <> who)` = genuine turn. Card abilities gated
  `controlled/restricted … (DuringTurn You)` are criteria, not windows.

**Gotcha:** the `AbilityWindow` ability-matcher is EXACT equality (`Game.hs:1870`,
`abilityWindow == windowMatcher`). Anything selecting basic actions by window —
`has{Fight,Evade,Investigate}Actions` and its callers — must query `DuringYourAction You`,
not `DuringTurn You`, or it silently finds nothing. Relates to [[project_musttakeaction_inversion_gap]].
