---
title: A bold action designator with no skill icon is still that action, with an automatically successful test
date_added: 2026-10-15
source: Alex Werner, FFG Game Rules Specialist (email reply)
affects:
  - Zburamoarte's Lair (13051b, revealed side of Descending Tunnel)
  - Vicious Blow (01025) / Vicious Blow (2) (02299)
  - Bold action designators without a skill symbol
  - SkillTestAutomaticallySucceeds
---

# A bold action designator with no skill icon is still that action, with an automatically successful test

**Q:** I have a question about actions with a bold action designator without a
corresponding symbol, like the one on Zburamoarte's Lair. Is this intended to
show that no test is taking place, or is it simply a typo? For example, am I
able to commit a Vicious Blow to this test?

**A (Alex Werner, FFG Game Rules Specialist):**

> The ability on Zburamoarte's Lair is a Fight action with an automatically
> successful skill test, which Vicious Blow can be committed to. There should
> have been a combat icon following the "Fight" bold action designator, and we
> are likely to errata that on the card.

So the missing icon is a misprint, not a signal that the ability skips the test.
The ability:

- **is** a Fight action (so it triggers everything keyed on performing a Fight,
  and `Vicious Blow`'s `PassedSkillTest iid (Just Fight)` rider applies),
- **does** initiate a real **{combat}** skill test, which merely succeeds
  automatically — the test still runs through ST.1–ST.8, so ST.2 happens and
  cards may be committed to it,
- is committable by anything whose icons are relevant to a combat test, which is
  the only gate on committing.

This is the general automatic-success rule applied to a Fight: FAQ v2.5 ruling
(2.9) and the Grimoire glossary entry "Automatic Failure/Success" both say the
test still takes place, ST.3/ST.4 are skipped when the success is known before
the reveal, "cards may still be committed to the test", and the investigator's
total modified skill value is still determined (the total difficulty is treated
as 0). The only thing this ruling adds is that the missing icon does not take
the test away.

The general principle: a bold action designator asserts that you are performing
that action. Where the designator is followed by no symbol *and* the ability
text supplies no test of its own, read the missing symbol as the omission it is
rather than as "no test takes place". (This is distinct from abilities like
`Parley.` ones that print no test and resolve purely as written — there the
absence is the design, not a dropped icon.)

## Affected cards / systems

- Zburamoarte's Lair (13051b) — `backend/arkham-api/library/Arkham/Location/Cards/ChildrenOfBlood/NewHorizons/DescendingTunnel.hs`
  (card def: `Arkham/Location/CardDefs/ChildrenOfBlood/NewHorizons.hs:93`)
- Vicious Blow (01025) — `backend/arkham-api/library/Arkham/Skill/Cards/ViciousBlow.hs`
- Vicious Blow (2) (02299) — `backend/arkham-api/library/Arkham/Skill/Cards/ViciousBlow2.hs`
- Engine: `fightAction` in `Arkham/Ability/Type.hs:39`, `mkChooseFightPure` in
  `Arkham/Fight.hs:20`, `fight` in `Arkham/Helpers/SkillTest.hs:283`,
  `SkillTestAutomaticallySucceeds` in `Arkham/SkillTest/Runner.hs:298`

## Implementation status

- **Zburamoarte's Lair (13051b)**: ✅ already matched the ruling — no change.
  Ability 2 is `fightAction (GroupClueCost (PerPlayer 1) (be a))`, i.e.
  `ActionAbility #fight #combat`, and its handler pushes a real
  `FightEnemy` built from `mkChooseFightPure` (whose `chooseFightSkillType` is
  `#combat`) with `SkillTestAutomaticallySucceeds` scoped to that test. The
  attack is therefore a combat test tagged `skillTestAction = Just Fight`
  (`Helpers/SkillTest.hs:292`), which is exactly what the ruling describes.
- **Commit window**: ✅ already matched. `SkillTestAutomaticallySucceeds` is read
  at `TriggerSkillTest` (`SkillTest/Runner.hs:298`), which is reached at ST.3 —
  *after* `BeforeSkillTest` sets `CommitCardsFromHandToSkillTestStep`
  (`:194`). Nothing in the commit path is gated on the auto-success modifier, so
  the test is offered for commitment normally and auto-success cannot
  short-circuit ST.2.
- **Vicious Blow (01025)**: ✅ already matched. `Do PassSkillTest`
  (`:457`) sets `SucceededBy Automatic` and falls through ST.6/ST.7 like any
  other success, so the bare `PassedSkillTest iid (Just Fight) _ (SkillTarget …)`
  that Vicious Blow listens for still fires and its `DamageDealt 1` lands on the
  attack.
- **Vicious Blow (2) (02299)**: ✅ already matched, and gives +2. `Do PassSkillTest`
  sets `SucceededBy Automatic modifiedSkillValue'` with the difficulty zeroed, so
  the `n` its handler tests against (`>= 2`) is the investigator's total modified
  combat value — which is the value FAQ (2.9) says is still determined on an
  automatic success. Nothing to change.
- **Other bold-Fight abilities**: ✅ surveyed. Every `fightAction` /
  `fightActionWith` / `fightAction_` user in the engine routes through a real
  fight test (`mkChooseFight*`, the scripted `fight` builder, or the enemy
  `Attrs` default). No card implements a printed Fight designator as a
  test-free effect.

## Tests

None added — no behavior changed. There is no spec for Descending Tunnel /
Zburamoarte's Lair; a focused one would drive ability 2 with a Vicious Blow
committed and assert the enemy takes base damage + 1 (+1 from the location's own
`DamageDealt 1`).
