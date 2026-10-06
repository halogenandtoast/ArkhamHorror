---
title: Flipping a Cthulhu facet deals it no damage with that same action
date_added: 2026-10-06
source: FFG rules team (email reply, quoted on BGG thread 3543990 "Errata on Cthulhu")
affects:
  - Cthulhu (Hoary Wings) (11702 / 11702b)
  - Cthulhu (Fierce Visage) (11703 / 11703b)
  - Cthulhu (Wicked Claw) (11704 / 11704b)
  - ReplaceEnemy / entry-tick suppression of reactions
---

# Flipping a Cthulhu facet deals it no damage with that same action

**Q: A Cthulhu facet's non-Enraged side reads "Cannot be damaged, defeated, or
exhausted" plus "[reaction] After you successfully fight or evade this enemy:
Flip it to its [[Enraged]] side." Its Enraged side reads "[reaction] After you
evade this enemy: Deal 1 damage to it." Success is determined at ST.6 and
results are applied at ST.7, so once the flip has happened, does the Enraged
side take the damage from the action that flipped it?**

A (FFG rules team):

> No; when you flip a Cthulhu card to its Enraged side, you cannot deal 1 damage
> to it with that same action, and it cannot be exhausted.

This is **not** an erratum — neither FAQ v2.5 (February 2026) nor Arkham
Grimoire v1.1 (July 2026) contains one for these cards; the only official text
touching them is the optional *Ultimatum of the Sleeper* in the FAQ's
Refractions. It is a direct rules-team answer, and it is the specific case of
the general rule already documented in
[[project_window_entry_tick_timing]]: a card that enters play during an open
window cannot respond to that window's already-occurred triggering condition.
The Enraged side enters play *inside* the after-evade window, so its own
"after you evade this enemy" reaction is not available for that evade.

The first fight or evade therefore costs the facet nothing. Note that the two
halves fail for *different* reasons, and neither is "the non-Enraged side's
`CannotBeDamaged` is still in play when the damage lands" — it is not:

- **Evade**: the 1 damage belongs to a reaction the freshly-flipped side may
  not take, because it entered play inside the window it would respond to.
- **Fight**: the flip resolves in an ST.6 after-window, but the attack's damage
  is not dealt until ST.7, by which point the Enraged side (which *can* be
  damaged) is the one in play. Nothing about the flip stops it on its own.

## Affected cards / systems

- Cthulhu (Hoary Wings) (11702) — `backend/arkham-api/library/Arkham/Enemy/Cards/TheDrownedCity/TheDoomOfArkham/CthulhuHoaryWings.hs`
- Cthulhu (Hoary Wings) Enraged (11702b) — `.../CthulhuHoaryWingsEnraged.hs`
- Cthulhu (Fierce Visage) (11703) — `.../CthulhuFierceVisage.hs`
- Cthulhu (Fierce Visage) Enraged (11703b) — `.../CthulhuFierceVisageEnraged.hs`
- Cthulhu (Wicked Claw) (11704) — `.../CthulhuWickedClaw.hs`
- Cthulhu (Wicked Claw) Enraged (11704b) — `.../CthulhuWickedClawEnraged.hs`
- Engine: `ReplaceEnemy` in `backend/arkham-api/library/Arkham/Game/Runner.hs`
  and the `respectsEntryTick` filter in `backend/arkham-api/library/Arkham/Helpers/Action.hs`

## Implementation status

- **Fight path**: ✏️ fixed on the three non-Enraged facets. The flip fires from
  the `SuccessfulAttackEnemy` `#after` window, which `SkillTestApplyResults`
  (ST.6, `SkillTest/Runner.hs:965`) pushes *ahead* of `SkillTestApplyResultsAfter`
  (ST.7, `:884`) — and ST.7 is where the bare `PassedSkillTest` reaches
  `Enemy/Runner.hs:1345`, registers the "Damage <enemy>" `SkillTestResultOption`,
  and ultimately pushes `InvestigatorDamageEnemy`. So the weapon's damage is
  dealt *after* the flip, to the Enraged side, and
  `sourceCanDamageEnemy` sees no `CannotBeDamaged`. Each facet's `Flip` handler
  now adds `withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs
  CannotBeDamaged`, scoping the immunity to the test that flipped it — "that
  same action" — and leaving it damageable on every later action.
- **Exhaust**: ✅ already matched. All six facets carry `DoNotExhaust` **and**
  `DoNotExhaustEvaded` since #5668; see
  [[project_donotexhaust_only_gates_attacks]].
- **Evade path**: ✏️ fixed in `ReplaceEnemy`. The handler minted the Enraged
  card with no `gameEntryTicks` entry, so `respectsEntryTick` fell through its
  `Nothing -> pure True` fail-open branch and the Enraged side's ability 2
  matched the *still-open* after-evade window (`ReplaceEnemy … Swap` keeps the
  enemy id, so `be a` matched). `ReplaceEnemy` now records
  `insertMap card.id (gameWindowTick g)`, which puts the new side's entry tick
  above the window's pinned `conditionTick` and suppresses the reaction. The
  `EnemyFlipped #after` window the non-Enraged side raises *after* the swap
  opens at a higher tick, so the Enraged side's Forced "after you flip this
  enemy to this side" ability still fires.

## Tests

`backend/arkham-api/tests/Arkham/Enemy/Cards/TheDrownedCity/TheDoomOfArkham/CthulhuWickedClawSpec.hs`
drives a real Fight against the printed facet under `scenarioTest "11688a"` with
Cthulhu's Rage set to 3, takes the flip reaction, and asserts 0 damage — plus a
control that fights the already-Enraged facet on its own action and asserts the
damage *does* land, so the immunity cannot silently become permanent. Wicked Claw
is the facet under test because its Enraged Forced ability ("place 1 doom on it")
is self-contained; Hoary Wings' draws from the Cthulhu deck, which the unit
harness does not set up.

The fix is deliberately in `ReplaceEnemy` rather than on the three cards: every
flip-side enemy built with `genCard` had the same hole. Surveyed users whose
replacement side carries a forced/reaction ability — Jean Devereux
(Seeking Closure/Possessed), Dagon, Hydra, The Bloodless Man, Amaranth,
Tzu San Niang, The Contessa, the Hemlock hybrids, and the Circus/Dark Matter
homebrew pairs — none depends on the old fail-open behaviour, and Jean
Devereux's `EnemyDefeated #when` pair is *corrected* by it: her
`insteadOfDefeatWithWindows` flip must not let the Possessed side's own
"when defeated" ability advance the act off the same defeat.
