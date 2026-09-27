---
name: project_donotexhaust_only_gates_attacks
description: DoNotExhaust does NOT mean "cannot be exhausted" — it only gates the attack exhaust; evade needs DoNotExhaustEvaded, and the generic Exhaust handler honours only CannotBeExhaustedBy (#5668)
metadata:
  type: project
---

There are three separate exhaust knobs and none of them is a general "cannot be exhausted":

- `DoNotExhaust` — read ONLY at the three attack push sites in `Arkham/Enemy/Runner.hs`
  (`Exhaust (mkExhaustion a a) | ... , DoNotExhaust \`notElem\` mods`). It means "attacking
  does not exhaust this enemy", which is how its non-Cthulhu users read it
  (`TheThroneRoom`, `ArkhamWoodsHiddenPath`, `Phantasmagoria`, `WatchersGrasp`,
  `TheChase`, `TheUnsealing` — several wrap it in `temporaryModifier … do <attack>`).
- `DoNotExhaustEvaded` — read in `Do (EnemyEvaded …)`, which feeds `evasionResult` →
  `successfulEvasion` (`Arkham/Message/Lifted.hs`) → `exhaustEnemy`.
- `CannotBeExhaustedBy SourceMatcher` — the only one the generic `Exhaust` handler
  consults. `DoNotExhaust` is invisible there, so once an `Exhaust` message exists
  nothing stops it.

A card printing a flat "Cannot be exhausted" therefore needs `DoNotExhaust` **and**
`DoNotExhaustEvaded` (and `CannotBeExhaustedBy AnySource` to be airtight).

#5668: *Cthulhu, Wicked Claw* (`11704`) had only `DoNotExhaust` while its five sibling
facets had both. Its own reaction is "after you evade this enemy: flip it to Enraged",
so the evade path was guaranteed. The exhaust landed on the non-Enraged side inside the
`Do (EnemyEvaded)` step, then `ReplaceEnemy … Swap` carried the flag onto the Enraged
entity — the card the player saw exhausted.

**Why:** the modifier name reads like an absolute ban but is a narrow, site-specific
gate; a card can look correctly implemented and still exhaust.

**How to apply:** when a card says "cannot be exhausted", grep for all three modifiers
and set every one the card's own triggers can reach. When adding a new exhaust push
site, remember it is gated at the call site, not centrally. See
[[project_aoo_gated_at_callsite.md]] for the same call-site-gating pattern.
