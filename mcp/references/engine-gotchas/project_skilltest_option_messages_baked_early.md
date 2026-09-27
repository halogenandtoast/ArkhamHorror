---
title: project-skilltest-option-messages-baked-early
description: "ST.7 options capture their messages when REGISTERED (PassedThisSkillTest), but execute after other options — state-dependent lists must be computed in a deferred DoStep"
---

`skillTestCardOption` / `additionalSkillTestOption` (`Arkham/Message/Lifted.hs`) `capture` their body
into a `SkillTestOption` at the moment the card handles `PassedThisSkillTest`. The option is only
**executed** later, after `CollectSkillTestOptions` presents the ordering ChooseOne and the player
resolves the other options (`Arkham/Game/Runner.hs` `SkillTestResultOptions`) — including the
investigate's own `OriginalOptionKind` "Discover Clue at …" option (`Location/Runner.hs`).

Anything the body reads from game state is therefore a snapshot from *before* the investigation's
clue discovery / damage / etc. The option's `criteria` field does not fix a stale *body*: it only
decides whether the option is offered. It IS re-checked before every ordering round, so it is the
right place for an availability gate — see [[project-st7-option-criteria-reevaluated-per-round]].

**Why:** Clean Sweep (#5315) computed `getAccessibleLocations` at `PassedThisSkillTest`, while the
investigated location still held its last clue. Passenger Car #169 is `Blocked` while unrevealed and
the location to its left has clues, so it was filtered out — and the eager `guard (notNull locations)`
would have suppressed the whole option in a two-location dead end. By the time the player picked the
option the clue was gone and the car was enterable, but the destination list was frozen.

**How to apply:** register the option with a `doStep n msg` body and do the state-dependent work in a
`DoStep n (<OriginalMessage>)` branch (idiom: `Event/Events/ImprovisedWeapon.hs`). Guard emptiness
inside that step (`unless (null xs)`) instead of before registering. Applies to any ST.7 option whose
content depends on state the test's own results will change. See [[project-onsucceedby-rider-repeat-skilltest]]
and [[project-afterskilltest-missing-ability-anchor]] for the neighbouring skill-test timing traps.
