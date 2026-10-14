---
title: project_a_card_that_stands_up_a_copy_republishes_its_own_abilities
description: An ability whose steps create a second entity from the same card gives that entity the same ability list — if its criterion asks about the CARD rather than the entity, the new copy fires it too, forever
---

`CreateSkill <fresh id> <card> <iid> <placement>` stands a skill up out of a
card (Surprising Find (1) does it from a search). The new entity publishes the
**same** `HasAbilities` list, because the abilities come from the card.

So an ability whose own steps create that entity will be published again on it.
Whether that loops depends on what its criterion asks about:

* a criterion about the **entity** -- its placement, its tokens -- is false for
  the new copy, and nothing happens;
* a criterion about the **card** is true for both, and the new copy fires the
  same ability, stands up another copy, and so on until `RunMessagesTimeout`
  turns the request into a 500.

`Criteria.IsCommitted` is the second kind: it reads the skill's card and asks
whether that card is in `InvestigatorCommittedCards`, so every copy of the card
answers the same. Laugh it Off ("if this test fails, put it into play") looped
on exactly this.

The fix is to make the card stop being committed as part of the same ability --
`SkillTestMessage (SkillTestUncommitCard_ iid card)` -- which is also what the
printed text means: a card put into play is no longer committed to the test.
Pushing it before the `CreateSkill` is enough; both land in the same flush.

An ability limit does *not* help here. `GroupLimit`/`PlayerLimit` filter
`usedAbilities` by whole-`Ability` equality, and the copy's ability differs in
`abilitySource`, so it is a different ability as far as the limit is concerned.
