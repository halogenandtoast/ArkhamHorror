---
name: project_onlyskillTestsource_never_replaced_you
description: OnlySkillTestSource was the one commit restriction that skipped replaceYouMatcher, so "an asset you control" resolved against the ACTIVE investigator — Crack Shot vanished from a seat's commit list as soon as another seat committed (#5862)
metadata:
  type: project
---

Every arm of `passesCommitRestriction` in `getIsCommittable`
(`Helpers/SkillTest.hs:691`) substitutes the committing investigator into its matcher —
`OnlyInvestigator` with `replaceYouMatcher a`, `OnlySkillTest` with
`replaceYouMatcher iid` — except `OnlySkillTestSource`, which passed its `SourceMatcher`
to `sourceMatches` raw. An unsubstituted `You` then falls through to `Game.hs:1267`,
which resolves it to **`gameActiveInvestigatorId`**.

The commit step asks every seat at once, and each commit re-points the active
investigator, so the restriction was evaluated against the wrong seat for everybody but
one. Crack Shot (11057, "a [[Firearm]] or [[Ranged]] asset **you control**") was offered
to Jenny while she was active, then **silently disappeared** the moment Carolyn committed
a card — no error, nothing in the log, just a shorter list (#5862). Fixed with
`replaceYouMatcher a matcher`; the twin arm in `Game.hs`'s `PassesCommitRestrictions`
card matcher uses `card.owner`, matching its own `OnlyInvestigator` neighbour.

**Why:** `cdCommitRestrictions` live on the *card def*, so there is no investigator in
scope to bind `You` at construction — the substitution is the caller's job, and the one
arm that forgot it looked correct because it reads like the card text. The default
`You` → active investigator makes it work in true solo and in any multiplayer test where
nobody else has committed yet, which is why it survived.

**How to apply:** `OnlySkillTestSource` has only two users, so the general lesson is the
dangerous one — a `You` reaching `select` unsubstituted is not an error, it silently
means "the active investigator". Grep for a matcher handed to `select`/`sourceMatches`
without `replaceYouMatcher` wherever the subject is not the active seat. Note the active
investigator is *not* the turn investigator and *not* the skill test performer
(here: active Carolyn, turn Jenny, test Jenny). Related:
[[project_forced_window_who_must_be_you]],
[[project_spawn_location_ask_comes_from_enemyspawnatlocationmatching]], and above all
[[project_playability_uses_active_investigator]] — the same default, already recorded.

**Reproducing it needs two seats and a prior commit.** Replaying the export straight, or
with `--undo 2` and only the test performer's answers, lands on the *good* state; the bug
only appears after injecting the other seat's commit, which flips the active investigator.
