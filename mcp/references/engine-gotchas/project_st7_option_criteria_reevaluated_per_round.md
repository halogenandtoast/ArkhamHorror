---
title: project-st7-option-criteria-reevaluated-per-round
description: "A SkillTestOption's `criteria` is re-evaluated before EVERY ST.7 ordering round, so availability gates belong there — an eager `whenM (can…)` in the PassedSkillTest handler suppresses the option forever"
---

`Arkham/Game/Runner.hs` `SkillTestResultOptions` builds the ordering prompt as
`opts & eachWithRest & mapMaybe \(opt, rest) -> pure $ uiAnd opt.option (SkillTestResultOptions rest)`.
Resolving one option therefore re-enters the same handler with the remaining options, and every
surviving option's `criteria` is checked again via `passesCriteria st.investigator Nothing st.source
st.source []`. An option whose criterion is false in round 1 **reappears** in round 2 if an earlier
result made it true.

This makes `criteria` the correct home for a *can I do this at all* gate, because ST.7 lets the
investigator order simultaneous results and an earlier result routinely lifts the blocker.

**Why:** Resourceful (#5534). William Yorick fought Graveyard Ghouls (`03017`, "while engaged with
you, cards cannot leave your discard pile") with a Chainsaw, committed Resourceful, and passed —
the attack defeated the Ghouls. Resourceful gated on `whenM (can.have.cards.leaveDiscard attrs.owner)`
*around* `skillTestCardOption`, so with the Ghouls still engaged at `PassedSkillTest` time no option
was ever registered. The player could never order "damage the Ghouls" ahead of it and get the return.

**How to apply:** register the option unconditionally with `skillTestCardOptionEdit attrs
(optionWhenExists <matcher>)` (`Arkham/SkillTest/Option.hs`) and put the whole gate in the matcher.
Capability matchers compose into it unapplied — `InDiscardOf (InvestigatorWithId attrs.owner <>
can.have.cards.leaveDiscard) <> basic (…)` collapses "owner can move cards out of their discard" and
"a legal card is there" into one `ExtendedCardExists`, the same shape as `Helpers/Criteria.hs`.
Prefer an investigator-explicit matcher: the runner evaluates criteria as `st.investigator`, which is
not the owner for a card committed to someone else's test. Pair it with a deferred `doStep n msg`
body — see [[project-skilltest-option-messages-baked-early]], whose "criteria does not help" note was
wrong about the re-evaluation. Sibling traps: [[project-onsucceedby-rider-repeat-skilltest]].
