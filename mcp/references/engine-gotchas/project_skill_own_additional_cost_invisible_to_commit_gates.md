---
title: project_skill_own_additional_cost_invisible_to_commit_gates
description: "A skill's own additional cost lives on the skill *entity*, which only exists from CommitCard onward — so no committability gate can see it and an unpayable cost throws InvalidState instead of hiding the card; declare it on the CardDef (cdAdditionalCost) instead (Justify the Means (3), #5671)"
---

A skill card's own additional cost used to be declared on the **entity**:

```haskell
justifyTheMeans3 =
  skillWith JustifyTheMeans3 Cards.justifyTheMeans3
    $ additionalCostL ?~ AddCurseTokensEqualToSkillTestDifficulty
```

The entity is created by `CommitCard` (`Game/Runner.hs`), which then unconditionally pushes
`PayForAbility (abilityEffect skill [] cost)`. **Every gate upstream of that runs before the
entity exists**, so none of them can see the cost:

- `getIsCommittable` (`Helpers/SkillTest.hs`) folds only `AdditionalCostToCommit` modifiers →
  the card is offered in the hand-commit list and to searchers like **Practice Makes Perfect**.
- `computeCommitCosts` (`SkillTest/Runner.hs`) folds only `AdditionalCostToCommit` + `CommitCost`,
  so `CheckAllAdditionalCommitCosts`' `getCanAffordCost` gate passes too.
- `ActiveCost.hs` then throws `InvalidState "Can't afford cost (b): …"` — a hard crash, not a
  refusal.

**Fix seam:** declare it on the `CardDef` (`cdAdditionalCost`) and let the `skill` builder in
`Skill/Types.hs` seed `skillAdditionalCost` from it. `Arkham.Card.CardDef` is reachable from
`Arkham.Helpers.SkillTest`; `Arkham.Skill` is **not** (every skill card module imports
`Arkham.Helpers.SkillTest`, so that direction is a module cycle). `getIsCommittable` now folds
`cdAdditionalCost` into its `getCanAffordCost` guard, skipping it when the card carries
`NoAdditionalCosts` (Amanda Sharpe's ability 2 waives commit costs that way).

Do **not** add it to `computeCommitCosts`: that list is both the affordability gate *and* what
`PayCommitCosts` pays, so including it there double-charges.

**Rules basis** (`rules/glossary/chaos_tokens.md`, Bless/Curse Q3): if adding bless/curse tokens is
part of a **cost** and there aren't enough left, the cost cannot be paid and the card cannot be
played/triggered. (As an *effect*, by contrast, you add as many as you can.)

**Recognising it (#5671):** `Costs [ActionCost 0, <cost>]` in the error text is the signature of
`abilityEffect skill [] cost` — a skill entity's own additional cost, not an ability's. The
`ActionCost 0` is the giveaway.

Two remaining declarations still use `additionalCostL` (Torrent of Power, Watch This, Watch This
(3), Soul Link); they are safe only because `UpTo` and `HorrorCost … YouTarget` are always
"affordable" per `Helpers/Cost.hs`. Anything with a genuinely refusable cost belongs on the def.

**Correction (#5678, 2026-09-11):** the gate must apply `cdAdditionalCost` **only when the card is a
skill** — `toCardType card /= SkillType` short-circuits it. For an asset or event `cdAdditionalCost`
is a cost of *playing* the card and is not paid to commit it for icons, and 40+ defs carry one.
Folding it in for every player card gated out Hemlock Vale's `ActionCost 1` events, Marie Lambeau (2)
and the Dream-Eaters bonded assets, and hard-crashed on Summoned Servitor: its
`CostIfCustomization` is only handled by `getCanAffordCost_`/`payCost` under a `CardIdSource`, and
the gate passes `toSource a` (an `InvestigatorSource`), so it hit `error "Not implemented"`
(`Helpers/Cost.hs:344`). Its one icon is willpower and the icon guard runs first, so *every*
willpower test in the game died.

**Follow-up (#5675, 2026-09-11):** a *forced* commit that waives the cost must push
`NoAdditionalCosts` **before** it calls `getIsCommittable`, not inside the `when committable`
branch. Amanda Sharpe's ability 2 did the check first, so the guard still folded in Justify the
Means (3)'s `AddCurseTokensEqualToSkillTestDifficulty`; against Sky Relic's difficulty-8 test with
7 curse tokens left in the pool the gate failed and her Forced commit **silently no-opped** —
no log line, no error, the card just stayed beneath her. Modifiers only land when the queue
processes the `CreateWindowModifierEffect`, so the fix is to split the handler: push the modifiers,
then `doStep 1 msg`, and do the `getIsCommittable`/`commitCard` in the `DoStep` branch.

Note also that `commitCard` pushes `SkillTestCommitCard`, which only registers the card in
`skillTestCommittedCards`. The skill **entity** (and `InvestigatorCommittedSkill`, which is what a
commit-trigger card like Justify the Means (3) listens for) is not created until
`CommitCardsFromHandToSkillTestStep` turns the registered cards into `CommitCard` messages — so a
state dumped during `SkillTestFastWindow1` legitimately shows the card committed with no skill
entity and no `SkillTestAutomaticallySucceeds` yet.
