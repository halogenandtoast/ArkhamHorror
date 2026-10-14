---
title: project_nothing_discarded_a_skill_entity
description: Discard on a SkillTarget reached no handler at all — a skill entity standing outside a test had no way to be discarded, and the message was silently ignored
---

`Discard (Maybe InvestigatorId) Source Target` has arms for assets, events,
treacheries, acts, agendas and searched cards. It had none for a **skill**, so
`Discard _ _ (SkillTarget sid)` fell through every runner and did nothing: no
error, no window, the entity still in play and the card nowhere.

It never came up because a skill entity normally only exists while committed to
a test, and `Do (SkillTestEnds)` sweeps those by placement. A card that stands
a skill up somewhere else -- `CreateSkill … (AttachedToLocation lid)`, or into
a play area -- then has no way to discard it.

`toDiscard`/`toDiscardBy` build the same message, so `RemoveAllAttachments` on
a skill was quietly inert too.

`SkillAttrs` now has the arm, modelled on the event's: the
`WouldBeDiscarded` batch, `Discarded` (which the skill already handled, by
removing itself from play), and `AddToDiscard` for the card. `RemoveFromPlay`
alone is not enough -- `RemovedFromPlay (SkillSource …)` is just `RemoveSkill`,
which deletes the entity and leaves the card nowhere.

Worth knowing when chasing this: the dev server can be running the previous
build's binary, so a correct arm can look inert. Compare the process start time
to the installed binary's mtime before concluding anything.
