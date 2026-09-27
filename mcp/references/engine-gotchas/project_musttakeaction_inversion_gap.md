---
title: project_musttakeaction_inversion_gap
description: MustTakeAction modifier uses an inverted preventsAbility check that suppresses non-action abilities unless guarded
---

`MustTakeAction x` (e.g. Haunted "next action must be Investigate") is interpreted via `not <$> preventsAbility x` in both `Helpers/Ability.hs` (ability triggering) and `Helpers/Action.hs` (basic actions). The `not` inversion means *any* ability whose actions don't include `x` is flagged prevented — which wrongly includes reaction/fast/free abilities that have no action at all.

**Why:** `preventsAbility (IsAction Investigate)` is only True when the ability's actions contain Investigate; `not` flips "no actions" into "prevented". Note `isFastAbility` returns False for `ReactionAbility`, so guarding on fast alone is insufficient.

**How to apply:** The triggering path must guard with `isActionAbility ability && not (isFastAbility ability || isReactionAbility ability)`; the basic-action path with `isFast == NotFast`. Fixed for #4799 (Court of the Great Old Ones suppressing Marie Lambeau's zap reactions). The Court location is currently the only `MustTakeAction` producer. Related: [[project_cannotbedamaged_investigator_gap]], [[project_removed_skills_not_outofgame]].
