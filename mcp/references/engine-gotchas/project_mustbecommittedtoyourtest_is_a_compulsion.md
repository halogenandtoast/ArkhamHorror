---
title: project_mustbecommittedtoyourtest_is_a_compulsion
description: "MustBeCommittedToYourTest names a compulsion (you must commit it to your own eligible tests), not a restriction; it must NOT filter eligibility or the card becomes invisible on other investigators' tests (Arrogance, #5384)"
---

`CommitRestriction`'s `MustBeCommittedToYourTest` is used by exactly two cards, both weaknesses whose text is a **compulsion**, not a restriction:

- **Arrogance (60303)** — "You must commit Arrogance to each eligible skill test *you perform*. This skill's icons subtract from your skill value instead of adding to it. If this test succeeds, return this skill to your hand."
- **Dream Parasite (06331)** — "While Dream Parasite is in your hand, you must commit it to the next skill test *you perform*, if able. …"

Neither says it may *only* be committed to your own test. Under the normal commit rules a skill card may be committed to a test performed by another investigator at the committer's location, and FAQ v2.5 Q&A #010 covers the split: abilities that alter the test itself (icons) benefit the performer, other abilities benefit the committer.

**The trap:** the constructor is consumed in two unrelated places, and it is easy to assume both are the same job.

1. **The compulsion (correct)** — `Investigator/Runner.hs` (`Do (CommitToSkillTest …)` and `DoStep 3 (CommitToSkillTest …)`) computes

   ```haskell
   mustCommit = any (elem MustBeCommittedToYourTest . cdCommitRestrictions . toCardDef) committableCards
   ```

   and drops the `StartSkillTestButton` / "done committing" trigger while it holds. This is already correctly scoped: it lives inside `when (iid == a.id)` and reads `getCommittableCards (toId a)`, i.e. the **performer's** own hand.

2. **Eligibility (was wrong)** — `passesCommitRestriction` treated it as a hard filter identical to `OnlyYourTest` (`pure $ iid == a`), in **both** copies: `Helpers/SkillTest.hs` (inside `getIsCommittable`) and `Game.hs` (the `PassesCommitRestrictions` card matcher). That made Arrogance invisible on anyone else's test. Both now `pure True`.

**Symptom to recognise (#5384):** at `CommitCardsFromHandToSkillTestStep` the exported `gameQuestion` is a bare `ChooseOne` keyed only to the performing player. When a non-performer *has* a committable card, `SkillTestAsk` (`Game/Runner.hs`) merges the two asks into an `AskMap` with a key per player — so a single-key question at a commit window means every other investigator was filtered out. Note the non-performer's branch has no "done" option of its own; the `AskMap` is resolved when the performer picks theirs.

Don't diagnose this as an icon problem: `#wildMinus` is a normal matching icon (`skillTestIconValues` carries `WildMinusIcon -1`), so `getSkillTestMatchingSkillIcons` accepts it.

**How to apply:** keep the constructor (it is serialised as part of `CardDef`, which homebrew card JSON depends on — don't rename or remove it), and keep it out of `passesCommitRestriction`. If a future card genuinely restricts committing to your own test, that's `OnlyYourTest`. Related: [[project_wrapper_ability_double_accounting]], [[project_skilltest_option_messages_baked_early]].

**Testing note:** on a *successful* test, Arrogance's `skillTestCardOption` return-to-hand adds an ST.7 ordering question ("Discover Clue at X" vs the card option) that must be answered before the success effects resolve — a spec that asserts clues straight after `applyResults` will read 0. Use `chooseFirstOption`. `commitFor` (in `TestImport/New.hs`) commits on behalf of a non-performer, whose options live under their own key in the question map.
