---
title: Aloof enemies cannot be attacked while unengaged — including "Fight" triggered abilities
date_added: 2026-06-03
source: Internal decision (clarifying the aloof keyword)
affects:
  - aloof
  - Lie in Wait
  - Whippoorwill
  - Dirty Fighting
---

# Aloof enemies cannot be attacked while unengaged — including "Fight" triggered abilities

An aloof enemy cannot be attacked if it is not engaged with an investigator. This includes
"Fight" bold-action triggered abilities — they do not bypass the aloof restriction. For example,
**Lie in Wait** cannot be triggered to attack an unengaged Whippoorwill in its location.

An ability that *would* allow an attack against an unengaged aloof enemy is worded very
specifically, usually as "Ignore the aloof keyword for this attack" (such as on **Dirty
Fighting**). Only such explicit wording ignores aloof.

This is consistent with the ArkhamDB Rules / Grimoire aloof glossary entry: "An investigator
cannot attack an aloof enemy while that enemy is not engaged with an investigator."

## Affected cards / systems

- aloof keyword fight gating — `canFightCriteria` / `canFightAtAnyLocation` in `backend/arkham-api/library/Arkham/Criteria.hs`
- Lie in Wait (60566) — `backend/arkham-api/library/Arkham/Event/Events/LieInWait.hs`
- Whippoorwill (02090, 05266, 12166) — `backend/arkham-api/library/Arkham/Enemy/Cards/Whippoorwill*.hs`
- Dirty Fighting (09073) / Dirty Fighting (2) — `backend/arkham-api/library/Arkham/Asset/Assets/DirtyFighting2.hs`

## Implementation status

- **aloof gating (`canFightCriteria`)**: ✅ already matched ruling. The default fight criterion wraps
  with `EnemyOneOf [not_ AloofEnemy, EnemyIsEngagedWith Anyone]`, so the standard Fight action
  already cannot target an unengaged aloof enemy.
- **`canFightAtAnyLocation` / Lie in Wait (60566)**: ✏️ updated. `canFightAtAnyLocation` (used only by
  Lie in Wait's triggered Fight ability) did not obey aloof, so the ability could be triggered to
  attack an unengaged aloof Whippoorwill. It now applies the same aloof wrap. (`backend/arkham-api/library/Arkham/Criteria.hs`)
- **Dirty Fighting (2)**: ✏️ updated (#5683). It applies the `IgnoreAloof` modifier for its attack,
  which is the explicit "ignore the aloof keyword" wording — but nothing read it: the modifier is
  attacker-scoped and the aloof clause in `canFightCriteria` only looked at the enemy. Evading an
  aloof enemy disengages it, so the queued fight action found no eligible ability and silently
  did nothing. `canFightCriteriaObeyAloof` now also passes on
  `InvestigatorExists (You <> InvestigatorWithModifier IgnoreAloof)`.
  (`backend/arkham-api/library/Arkham/Criteria.hs`)
- **Test**: added `LieInWaitSpec` asserting the fight reaction is *not* offered when an unengaged
  aloof Whippoorwill enters, but *is* offered for a non-aloof enemy. (`backend/arkham-api/tests/Arkham/Event/Events/LieInWaitSpec.hs`)

## Open questions / out of scope

- **Springfield M1903 (4)** (tabooed `TabooList19` variant) uses `canFightOverride` / `fightOverride`
  matchers that gate on `not_ (enemyEngagedWith iid)` but do **not** exclude aloof, so it has the same
  class of gap (it could snipe an unengaged aloof enemy). Springfield's card text does not say
  "ignore aloof," so per this ruling that is likely incorrect — but it is a separate taboo'd weapon
  outside the named scope of this ruling, and there may be other ranged weapons (e.g. Telescopic Sight)
  in the same boat. Flagged for the user rather than changed here. (`backend/arkham-api/library/Arkham/Asset/Assets/SpringfieldM19034.hs`)
