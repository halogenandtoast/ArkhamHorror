---
name: project_auto_success_must_wait_for_trigger_skill_test
description: "An on-commit auto-success must set SkillTestAutomaticallySucceeds, not call passSkillTest inline — Three Aces ended the test mid-commit-window and orphaned every card committed after it (issues #5540, #5547); CommitCard now refuses to run with no active skill test"
metadata: 
  node_type: memory
  type: project
  originSessionId: d81ced37-571f-4cea-aab1-ee0c49647e61
  modified: 2026-08-28T13:45:44.148Z
---

`passSkillTest` from inside `InvestigatorCommittedSkill` resolves the **whole** test
immediately, while the commit window is still open and `TriggerSkillTest_` is still queued
behind it. Three Aces (1) (`06199`) did this; the export for #5540 showed the queue as

```
SkillTestEnds_ , … , DrawCards(3) , TakeResources(3) , Run[CommitCard All In] , … , TriggerSkillTest_
```

so All In (5), committed after the third Three Aces, joined a test that no longer existed:
it never became a subscriber (no `PassedSkillTest`, no draw 5, missing from
`SkillTestResultOptions_`) and was never discarded — its skill entity sat in
`placement: Limbo` forever. `TriggerSkillTest_` then ran against a dead test (silently at
the Game level; `SkillTestResultOptions` on the same path `error`s "missing skill test",
`Game/Runner.hs:2453`).

Fix: set the modifier and let the engine apply it at ST.5.

```haskell
skillTestModifier stId attrs stId SkillTestAutomaticallySucceeds
```

`SkillTest/Runner.hs` `TriggerSkillTest` turns that into `pushAll [PassSkillTest,
UnsetActiveCard]` without `RequestChaosTokens` — which is also what "do not reveal chaos
tokens from the chaos bag" wants. Same seam Zeal/Hope/Augur use.

The card's "Then draw 3 / gain 3" then rides `PassedSkillTest … (isTarget attrs -> True)`,
pushed **directly** (not via `skillTestCardOption`): that lands at ST.6 t5A, ahead of the ST.7
option prompt where Perception's and All In's "if this test is successful" riders live. A
`MetaModifier "ThreeAces1.Resolves"` on the triggering copy keeps it once-per-test.

**Why:** committing is a step, not a moment. Anything that ends the test from inside the
commit window silently strands every later commit, and the damage is invisible in the UI —
the card just leaves your hand.

**How to apply:** a card that reads "that test automatically succeeds" must never call
`passSkillTest` at commit time. Symptom to grep for: a leftover entry in
`gameEntities.skills` with `placement: Limbo` after a test, or an
`SkillTestResultOptions_` list missing options for cards you know were committed. Keep a
fallback (`getsSkillTest (.step) >= RevealChaosTokenStep ⇒ passSkillTest`) for commits that
arrive after ST.5, e.g. Isabelle Barnes. Cards can only import `getSkillTest`/`getsSkillTest`
via `Arkham.Helpers.SkillTest` (it re-exports them `{-# SOURCE #-}`); importing
`Arkham.GameEnv` directly is a module cycle. See
[[project_skilltest_option_messages_baked_early]] and
[[project_st7_option_criteria_reevaluated_per_round]].

**Update (#5547, dup of #5540):** the same report came back with Deduction (2) (`60275`, the
alternate printing of `02150`) as the stranded card. Two follow-ups landed:

- `Game/Runner.hs` `CommitCard` now bails on `isNothing (g ^. skillTestL)` as well as
  `alreadyCommitted`. A commit that arrives after its test died is a no-op, so the card
  stays in hand instead of being destroyed — a safety net under every future variant.
- `Skill/Cards/JustifyTheMeans3.hs` (`07306`, "This test automatically succeeds") was still
  calling `passSkillTest` inline; ported to the same modifier + step-guard shape. Its
  `cdCommitTrigger = True` meant it could only strand *other* trigger cards in the batch,
  but the hole was real.

Batch ordering matters when reasoning about who gets stranded:
`CheckAllAdditionalCommitCosts` (`SkillTest/Runner.hs`) partitions on `cdCommitTrigger`, and
because `push` prepends, execution is `PayCommitCosts → noTriggerCommits (in commit order) →
triggerCommits → CommittedCards windows`. Three Aces and Deduction are both
`cdCommitTrigger = False`, so all four ran in one `pushAll` and everything after the third
Three Aces died.

Self-healing detail worth telling reporters: `Do (SkillTestEnds …)` sweeps **every** `Limbo`
skill, not just the ending test's, so an already-stranded card returns to the discard at the
end of the player's next skill test.

**A reveal-window auto-success (Secluded Tent, `:circus-ex-mortis:054`).** The modifier is
read in exactly one place — `SkillTest/Runner.hs` `TriggerSkillTest` (ST.5). Nothing
downstream consults it: `calculateSkillTestResultsData` knows only `FailTies`,
`SkillTestResultValueModifier` and `AutomaticallyFailIfSucceedByAtLeast`. So
`skillTestAutomaticallySucceeds` set from a chaos token reveal window is a silent no-op —
the ability fires, the log records it, the test still fails.

`passSkillTest` from that window does work, and it also **skips ST.4 entirely**. At the
`#when (RevealChaosToken …)` window (`SkillTest/Runner.hs` `RequestedChaosTokens`) the step
is still `SkillTestFastWindow2`, and `RevealSkillTestChaosTokens` — which builds the
`Will (ResolveChaosToken …)` batch — is queued *behind* the window. `Do PassSkillTest`'s
`chooseOne [SkillTestApplyResultsButton]` blocks in front of that batch; applying results
ends the test, so the revealed token's symbol effect never resolves.

For Circus Ex Mortis' ☾ token ("0. Seal this token on your investigator card and reveal
another token") the owner confirmed the ruling: **you still seal the token, but stop
drawing.** So skipping ST.4 is exactly right for the draw, and the card performs the seal
itself, in order, before ending the test:

```haskell
UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
  sealChaosToken iid iid token
  passSkillTest
```

Do **not** move this to `SkillTestStep #after ResolveChaosSymbolEffectsStep` (the
`CrypticGrimoireTextOfTheElderHerald4` seam). That window is pushed after the
`Will (ResolveChaosToken …)` batch, so the extra token has already been drawn by then —
which is the half the ruling says must not happen.

Do **not** teach `RunSkillTest` to honour the modifier at ST.6 either: it is a hot path and
the owner wants behaviour there left alone.

So, by when the auto-success becomes known:

- **Before ST.5** (commit window, "when this test begins", on-play) → set the modifier.
- **At a chaos token reveal** → `passSkillTest`, and hand-resolve whatever part of the
  token's own effect the ruling says still happens.
- **After ST.4 already resolved** (a late commit, `SkillTestStep #after RevealChaosTokenStep`)
  → `passSkillTest` directly. Models: `StrokeOfLuck2`, `Beloved`, `ParadoxicalCovenant2`.

`passSkillTest` is safe after ST.3 because `Do PassSkillTest` zeroes `difficultyL`, so a
`RunSkillTest` still queued behind it recalculates to a success anyway.

Still-broken instance of the same class, not fixed:
`Campaigns/TheFeastOfHemlockVale/TokenHelpers.hs` `hemlockPreludeResolveChaosToken` sets
`SkillTestAutomaticallySucceeds` from Cultist-on-parley, doubly dead — it runs at
`ResolveChaosToken` *and* targets the `ChaosToken`, which `getModifiers (SkillTestTarget
sid)` never reads.
