---
title: project_ignorealoof_is_matcher_only
description: "IgnoreAloof is honoured by the EnemyWithKeyword/AloofEnemy matcher but NOT by the keyword layer (getModifiedKeywords / field EnemyKeywords), so an enemy-targeted IgnoreAloof never engages; 'loses aloof' must be RemoveKeyword Aloof (#5392)"
---

`IgnoreAloof` and `RemoveKeyword Aloof` look interchangeable and are not. They are read by two
different layers, and only one of them is consulted by the engagement path.

**Matcher layer — honours `IgnoreAloof`.** `EnemyWithKeyword` (`Arkham.Game` ~4004, and therefore
the `AloofEnemy` pattern) reads `getModifiers` on the enemy and filters `Keyword.Aloof` out when
either `IgnoreAloof` or `RemoveKeyword Aloof` is present.

**Keyword layer — does NOT.** `field EnemyKeywords` (`Arkham.Game` ~4954) applies only
`AddKeyword` / `RemoveKeyword` / `LosePatrol` / `ForcePatrol`. `getModifiedKeywords`
(`Arkham.Helpers.Enemy` ~155) wraps that field and only rewrites `Swarming`. Neither knows about
`IgnoreAloof` (or `IgnoreRetaliate`).

Every engagement decision reads the **keyword** layer:

- `EnemyCheckEngagement` (`Enemy/Runner.hs` ~866) → `getModifiedKeywords` → guard
  `none (elem keywords) [#aloof, #massive]`.
- `Will (EnemyEngageInvestigator …)` (`Enemy/Runner.hs` ~1994) → same call → `unless (… || Aloof elem kws)`.

So an enemy carrying only `IgnoreAloof` is "not aloof" to every matcher and "still aloof" to every
engagement check. The disagreement defeats the existing safety net: `handleAloofChanges`
(`Arkham.Game` ~6757) diffs `AloofEnemy` before/after each message and pushes
`EnemyCheckEngagement` for anything that stopped being aloof — the handler then throws it away.
`BeginRoundWindow` and `After (EndTurn _)` (`Enemy/Runner.hs` ~860-861) push the same message every
round and fail identically, so the enemy stays unengaged forever.

**Rule of thumb**

- "**Each X enemy loses aloof**" (a property of the enemy, from an agenda/act/location/treachery)
  → `RemoveKeyword Aloof`. This is what `Act/Cards/TheFirstOath.hs`, `TheThirdOath.hs`,
  `Location/Cards/Office.hs`, `TempleCourtyard.hs`, `Treachery/Cards/InconvenientQuesitoningA-D.hs`
  already do.
- "**You ignore aloof when you fight**" (a property of the attacker, scoped to a skill test)
  → `IgnoreAloof`, applied to the **investigator / skill-test** target, never to the enemy —
  British Bull Dog (2), Longbow (3), Dirty Fighting (2), Telescopic Sight (3), Marksmanship (1),
  Enchanted Bow (2), Summoned Servitor. Pair it with
  `CriteriaOverride canFightIgnoreAloof` (`Arkham.Criteria`) so the fight action is offered at all.

#5392: the four In Too Deep agendas (Barricaded Streets `07124`, Relentless Tide `07125`, Flooded
Streets `07126`, Rage of the Deep `07127`) all read "Each [[Suspect]] enemy loses aloof and cannot
be parleyed with" but used `IgnoreAloof`. Zadok Allen sat unengaged at the investigators' location
indefinitely. Fixed by switching all four to `RemoveKeyword Aloof`; no card applies `IgnoreAloof`
to an enemy target any more, so the special case in `EnemyWithKeyword` is now dead code.

**Criterion layer — now honours attacker-side `IgnoreAloof`.** `canFightCriteriaObeyAloof`
(`Arkham.Criteria`) holds the aloof gate for the enemy's basic Fight ability
(`Enemy/Types.hs`, `AbilityAttack`). Until #5683 its clause was purely enemy-side
(`EnemyOneOf [not_ AloofEnemy, EnemyIsEngagedWith Anyone]`), so an investigator carrying
`IgnoreAloof` was still blocked. It is now `aloofFightRestriction`, which also passes on
`InvestigatorExists (You <> InvestigatorWithModifier IgnoreAloof)`.

This matters because a `CriteriaOverride` does **not** reach that check. `handlePerformAction`
(`Investigator/Runner/Action.hs`) filters fight abilities with a bare `getCanPerformAbility` —
`EnemyFightActionCriteria` overrides are only read inside the `CanFightEnemy*` *matcher*
(`Arkham.Game`) and by `canDoAction'` for non-enemy sources. So a card that pushes
`PerformAction … #fight` (Dirty Fighting (2), Daniela Reyes (2)) can only get past aloof via the
attacker-side modifier; cards that build their own `mkChooseFightMatch` (British Bull Dog (2),
Longbow (3)) go through the matcher and can use the override. `MustFight` is likewise
matcher-only, so it cannot rescue an ability that was already filtered out.

If you add a disjunction to a fight criterion, also teach `standardFightCriterion`
(`fightOffersAsIfEnemyTargets`, `Arkham.Criteria`) about it — an unhandled constructor falls to
`_ -> False` and silently drops as-if-enemy targets. See
[[project_fight_override_is_a_widening_not_a_narrowing]].

#5683: evading the aloof Seeker of Carcosa disengaged it, so Dirty Fighting (2) exhausted and the
queued `PerformAction … Fight` found no eligible ability and no-op'd with no log line.

**How to apply:** when a card grants/removes a keyword as a lasting property of the enemy, use
`AddKeyword`/`RemoveKeyword` — those are the only ones the keyword layer understands. Reserve the
`Ignore*` modifiers for attacker-scoped "ignore the effect of this keyword" wording.
Related: [[project_after_enter_engagement_timing]], [[project_enemy_movement_placement]],
[[project_fight_override_is_a_widening_not_a_narrowing]].
