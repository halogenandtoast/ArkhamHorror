---
title: project_removed_skills_not_outofgame
description: Skills removed from game (OutOfPlay RemovedZone) still match broad skill matchers; getSkillsMatching only filters OutOfGame
---

`getSkillsMatching` (`Arkham/Game.hs` ~3335) only drops skills whose placement is
`OutOfGame _` (`Placement.outOfGame`, `Arkham/Placement.hs:58`). It does **not** drop
`OutOfPlay RemovedZone`. So a broad matcher like `NotSkill (SkillWithId attrs.id)` still
matches skill *entities* that lingered in `entitiesL.skillsL` after being removed from game —
notably **Three Aces** (Myriad: committing 3 copies auto-passes and applies
`SetAfterPlay RemoveThisFromGame`, leaving the entities in `RemovedZone`).

**Why:** This caused issue #4788 — On the Brink's failure handler
(`Skill/Cards/OnTheBrink.hs` / `OnTheBrink2.hs`) did `selectEach (NotSkill $ SkillWithId attrs.id)`
+ `returnToHand`, which pulled the removed Three Aces back into hand.

**How to apply:** When a card's effect should only touch in-play / committed skills, add the
`SkillNotRemoved` matcher (which excludes `OutOfPlay RemovedZone`), e.g.
`NotSkill (SkillWithId attrs.id) <> SkillNotRemoved`. `HelpingHand.hs:20` uses the same broad
`NotSkill` pattern but only to grant a modifier, so it's harmless there.
