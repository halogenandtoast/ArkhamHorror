---
title: project_committed_skill_entity_id_is_not_the_card_id
description: A committed skill is a real Skill entity with a random id; every other committed card type is a pseudo-entity keyed by the card's own UUID, so an id-based identity check is right for three kinds and silently wrong for the fourth
---

`preloadCommittedEntities` stands a committed asset, event or treachery up as a
pseudo-entity whose **id IS the card's UUID**. A committed *skill* is the
exception: `CommitCard` creates a genuine `Skill` entity with a **random**
`SkillId` and placement `Limbo`.

So a check written as

```haskell
unsafeToCardId eid `elem` committedCards
```

is correct for the three pseudo-entity kinds and quietly false for a skill. The
skill's card has to be read off the entity instead:

```haskell
SkillSource sid -> maybe False ((`elem` committed) . toCardId) <$> fieldMay SkillCard sid
```

`Criteria.IsCommitted` was written the first way, so every "while this is
committed" ability on a *skill card* was unreachable -- the one card type where
that wording is most common. There is no error and no log line; the ability
simply never fires, and the only tell is that nothing happens.

Found across 16 custom printings at once (Circus Ex Mortis: Concentration,
Fluke?, Escalation, Synchronized, Court Death, Breathtaking, Team Effort,
Laugh it Off, at two levels each). It took a `TakeResources` probe as the
ability's first step and watching the resource count not move.

The same asymmetry matters at teardown: `Do (SkillTestEnds)` in
`Game/Runner.hs` sweeps exactly the skills whose placement is `Limbo`, so a
skill a card stands up in `InPlayArea` survives the test while the committed
one does not.
