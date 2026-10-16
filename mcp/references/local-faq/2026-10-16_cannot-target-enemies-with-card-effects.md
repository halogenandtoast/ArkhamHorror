---
title: "Cannot target enemies with card effects" has no engine primitive
date_added: 2026-10-16
source: internal decision (The Invisible Man implementation)
affects:
  - Lonely Farmstead
  - Panicked Flight
  - Shadowed Escape
  - cannot target
  - card effect
---

# "Cannot target enemies with card effects" has no engine primitive

There is no `CannotTarget` modifier, and no single place where the engine can
tell "a card effect is choosing an enemy to target" apart from "something is
counting or testing for enemies". `select` on an `EnemyMatcher` answers both, so
filtering it would silently change every `if there is an enemy at your location`
condition. The clause is therefore spelled out of the existing vocabulary, and
the residue is accepted rather than hidden.

The two seams that look like they ought to carry it, and do not:

- `InvestigatorCanTarget` (`library/Arkham/Matcher/Investigator.hs:196`) is
  matched in `library/Arkham/Game.hs:1673`, where it handles
  `EncounterDeckTarget` and otherwise falls through to `pure True`. One call
  site, in `library/Arkham/Capability.hs:55`.
- `CannotAffectOtherPlayersWithPlayerEffectsExceptDamage` shows the shape a
  working version takes: a capability each targeting card opts into via
  `affectsOthers` (`library/Arkham/Matcher.hs:95`), not a central filter.

## The ruling

"You cannot target enemies with card effects" is implemented as the union of
`CannotFight`, `CannotTriggerAbilityMatching (AbilityOnEnemy ...)` and, where
the card also protects the enemy from harm, `CannotBeDamagedByPlayerSources`.
An activated ability that targets an enemy **is** a card effect for this
purpose. Evading is **not** — it is neither an attack nor a card effect — so a
card that bans attacks and card effects without naming evasion leaves evading
legal.

What still slips through: a player card that touches an enemy by some route
other than fighting it, triggering an ability on it, or dealing it damage. No
such card is known to break either scenario, but the gap is real.

## Affected cards / systems

- Lonely Farmstead (`:the-invisible-man:015`) —
  `backend/arkham-api/library/Arkham/Homebrew/TheInvisibleMan/Locations/LonelyFarmstead.hs`
- Panicked Flight, Shadowed Escape — via `griffinBreaksAwayFrom` in
  `backend/arkham-api/library/Arkham/Homebrew/TheInvisibleMan/Helpers.hs`
- `Arkham.Modifier`: no `CannotTarget` constructor exists

## Implementation status

Implemented as described. Lonely Farmstead carries `CannotBeEngaged`,
`CannotFight AnyEnemy`, `CannotTriggerAbilityMatching (AbilityOnEnemy AnyEnemy)`,
`CannotMove` and `CannotBeMoved`, and deliberately no `CannotEvade`.
`griffinBreaksAwayFrom` additionally bans evading Griffin, because those cards
name the evasion attempt themselves.
