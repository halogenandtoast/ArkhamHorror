---
name: project_committed_skills_rider_needs_explicit_check
description: "\"If you committed 1 or more skills to this skill test\" needs an explicit `select $ skillOwnedBy iid` gate; Silas's Net offered its free evade (and its return-to-hand prompt) with zero commits (#5549)"
metadata:
  type: project
---

Only two cards carry the clause **"if you committed 1 or more skills to this skill test"**:
Sea Change Harpoon (`07014`) and Silas's Net (`07015`).

The Harpoon gets it for free — its rider is a modifier,
`OnCommitCardModifier iid #skill (DamageDealt 1)`, which simply never fires without a commit.
Silas's Net's rider is a *prompt* ("you may automatically evade another enemy engaged with
you"), so it needs an explicit check — and had none. `PassedThisSkillTest` only checked
"passed" + "another engaged enemy exists", so a no-commit evade handed out a free automatic
evade (#5549: `committedCards: {}`, `SucceededBy 3`, prompt shown).

The same card's **`SkillTestEnds`** handler had the mirror gap: it offered "return Silas's Net
to your hand to return all of your committed skill cards" with an empty skill list, i.e. a free
bounce of the asset back to hand. Sea Change Harpoon still has this second gap (left for a
separate issue).

The right predicate is `select $ skillOwnedBy iid`:

- `skillOwner` is the **committing** investigator, not the deck owner — set at entity creation
  (`Skill/Types.hs`), and `Game/Runner.hs` `Do (SkillTestEnds …)` says so in a comment.
- Skill entities exist only while committed and are torn down in `Do (SkillTestEnds …)`
  (`Game/Runner.hs:1527`) — **after** both `PassedThisSkillTest` (ST.7) and `SkillTestEnds`
  (ST.8) reach the card, so the list is still populated in both handlers. Do not reuse the
  predicate after that point.
- Only skill-type committed cards become Skill entities (`SkillTest/Runner.hs` discards
  non-skill committed cards separately), so it correctly excludes committed events/treacheries,
  which the card text does not count.
- `SkillOwnedBy` filters on owner only (`Game.hs:3593`), with no placement filter.

**Known unfixed edge:** Silas Marsh's own reaction returns a committed skill to hand mid-test,
which removes both the Skill entity and the `committedCards` entry — so the rider is suppressed
even though the player did commit. `History.hs` has no committed-cards field, so there is no
record to consult.

**How to apply:** for any "if you committed …" rider expressible as a modifier, prefer
`OnCommitCardModifier`; for a prompt, gate on `notNull <$> select (skillOwnedBy iid)` inside the
ST.7/ST.8 handler. See [[project_skilltest_option_messages_baked_early]] and
[[project_passedskilltest_type_is_post_mutation]].
