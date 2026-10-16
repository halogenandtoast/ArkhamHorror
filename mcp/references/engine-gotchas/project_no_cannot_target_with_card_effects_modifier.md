---
title: project_no_cannot_target_with_card_effects_modifier
description: The engine has no "cannot target with card effects" modifier — CannotFight/CannotEvade/CannotTriggerAbilityMatching/CannotBeDamagedByPlayerSources only approximate it, and the residue is real
---

There is no `ModifierType` for "you cannot target enemies with card effects" —
zero hits for `CannotTarget` in `Modifier.hs` or anywhere in the tree. Cards
printing that sentence are spelled out of the permissions the engine does
honour:

    roundModifiers iid
      [ CannotFight (be eid)
      , CannotEvade (be eid)
      , CannotTriggerAbilityMatching (AbilityOnEnemy $ be eid)
      ]
    roundModifiers eid
      [ CannotEngage iid
      , CannotBeDamagedByPlayerSources (SourceUsedBy $ InvestigatorWithId iid)
      ]

Precedents: `Homebrew/AgesUnwound/Treacheries/OutOfPhase.hs` (the same
"interact" problem), `Asset/Assets/CowlOfSekhmetCloakOfPharaohs3.hs`,
`Treachery/.../WordsOfPower.hs`.

**The residue is real.** A card that touches the enemy some other way — not a
fight, not an evade, not an ability, not player-source damage — is not blocked.
Lonely Farmstead and Panicked Flight / Shadowed Escape (The Invisible Man) all
ship with that gap; so does anything else printing the phrase. Closing it needs
a new `ModifierType` honoured wherever card effects select an enemy target,
which is a core change, not a card change. Note also
`CannotTriggerAbilityMatching` exempts Forced and basic abilities
([[project_cannot_trigger_ability_matching_exemptions]]).

**Why there is no chokepoint.** Two seams look like they should carry it and do
not. `InvestigatorCanTarget` (`Matcher/Investigator.hs:196`) is matched in
`Game.hs:1673`, where it handles `EncounterDeckTarget` and otherwise falls
through to `pure True`; it has one call site, `Capability.hs:55`. And
`CannotAffectOtherPlayersWithPlayerEffectsExceptDamage` only works because each
targeting card opts in through `affectsOthers` (`Matcher.hs:95`). A blunt filter
on `select` of an `EnemyMatcher` is wrong, because the same query answers
conditions and counts ("if there is an enemy at your location"), not just target
choices.

**Settled reading** (2026-10-16, author's call, recorded in
`local-faq/2026-10-16_cannot-target-enemies-with-card-effects.md`): an activated
ability that targets an enemy IS a card effect, so the bundle includes
`CannotTriggerAbilityMatching`. Evading is NOT — neither an attack nor a card
effect — so a card banning "attacks or card effects" without naming evasion
leaves evading legal. Lonely Farmstead follows exactly that; the Griffin
break-away helper also bans evading because those cards name the evasion
attempt themselves.
