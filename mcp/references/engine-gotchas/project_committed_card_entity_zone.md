---
name: project_committed_card_entity_zone
description: "Only a committed *skill* gets an entity for free; anything else needs `CommittedEffect` + `Criteria.IsCommitted` to act from the skill test, served by `gameCommittedEntities`"
metadata:
  type: project
---

Committing a card files it under `skillTestCommittedCards` and `ObtainCard`s it out of its old
zone. A committed **skill** keeps an entity (`InvestigatorCommittedSkill` parks a `Skill` in
`Limbo`); an asset, event or treachery keeps nothing, so "while this is committed" was not
expressible as an ability at all. `pendingCommitEntities` (`Game.hs`) looks like the hole but is
not: it is built inside `preloadModifiers`, thrown away with it, and its own haddock says it
never reaches `getAbilities` or `runMessage`.

As of 2026-10-09 there is a real zone, mirroring `InHandEffect` one-for-one:

- `cdOutOfPlayEffects = [CommittedEffect]` on the def opts the card in.
- `preloadCommittedEntities` (`Game/Runner.hs`) rebuilds `gameCommittedEntities` from
  `skillTestCommittedCards` before every message, placement `Limbo`, controller = the committing
  investigator, keyed by `unsafeCardIdToUUID` — **so the entity id IS the card id**.
- Dispatch is `Committed iid msg`, so handlers read `Committed iid (UseThisAbility …)`; the
  `UseAbility` → `Do msg` unwrap arm exists in the Asset, Event and Skill runners.
- `getGameAbilities` surfaces them only when the criteria carry `Criteria.IsCommitted`
  (`committedAbility`, the same guard shape as `inHandAbility`/`InYourHand`).
- A card already loaded elsewhere is skipped, so an in-play copy and a committed copy are never
  both live — that is the #5555/#4764 double-resolution trap.
- `gameCommittedEntities` is derived state: decode is `.:? … .!= mempty`, and the multiplayer
  merge in `Shared.hs` is deliberately left alone.

**No frontend change was needed.** `CommittedSkills.vue` renders a non-skill committed card with
`Card.vue`, whose `isAbility` falls through to `source.contents === cardId` when the ability's
asset is not in `game.assets` — which matches exactly because the entity id is the card id.

**How to apply:** for "while committed" abilities, add `CommittedEffect` and gate on `IsCommitted`.
Committing from *play* is a separate problem: `getCommittableCards` only reads hand plus
`CanCommitToSkillTestsAsIfInHand`, so an in-play card grants itself that for the test, adds
`MustBeCommitted` so `CommitToSkillTest` auto-commits it at ST.2, and leaves play on `CommitCard`
(guarded by `isInPlayPlacement`, or the committed entity re-fires it). Yin's Drumsticks
(Symphony of Erich Zann) is the worked example; Amanda Sharpe is the in-test precedent and adds
`LeaveCardWhereItIs` to dodge the end-of-test discard.

Related: [[project_committed_skills_rider_needs_explicit_check]],
[[project_cards_beneath_asset_have_no_entity]], [[project_st7_option_criteria_reevaluated_per_round]].
