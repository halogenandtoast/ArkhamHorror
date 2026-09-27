---
name: project_transfigured_form_field_overrides
description: "TransfiguredForm (Transfiguration (2), 11076) overrides printed values only in the Game.hs field projections, one field at a time — InvestigatorClass was missed and reported the original investigator's class (#5544). The raw attrs (and the wire) keep the original values"
metadata: 
  node_type: memory
  type: project
  originSessionId: a593ce32-754c-43d0-be70-f19ebc79c910
  modified: 2026-08-29T01:56:04.988Z
---

Transfiguration (2) (`11076`): "treat the front of your investigator card as if it were
the front of the chosen card, instead *(including your skill values, traits, abilities,
and elder sign effect)*." The parenthetical is **not exhaustive** — everything printed on
the front changes, class symbol included (confirmed by the user, 2026-08-29).

Two separate mechanisms implement this, and neither is automatic:

1. **Dispatch** (`Investigator/Types.hs`, `Investigator/Runner.hs`): `HasAbilities`,
   `HasModifiersFor`, `HasChaosTokenValue` and `RunMessage` re-dispatch to the inner
   investigator via `withInvestigatorCardCode inner` + `asFormAttrs` (which swaps
   `investigatorMeta` for `investigatorFormMeta` — see
   [[project_form_meta_invisible_to_projections]]).
2. **Field projections** (`Arkham/Game.hs`, the `Investigator*` field case block): each
   printed value must be overridden **by hand**, one `TransfiguredForm inner ->` branch
   per field. `InvestigatorHealth`, `InvestigatorSanity`, `InvestigatorBaseWillpower`
   / `BaseIntellect` / `BaseCombat` / `BaseAgility` use
   `lookupInvestigator (InvestigatorId inner) investigatorPlayerId`;
   `InvestigatorTraits` uses `cdCardTraits` from `allInvestigatorCards`.

`InvestigatorClass` was missed and fell through to `pure investigatorClass` — a Luke
Robinson (Mystic) transfigured into Norman Withers (Seeker) still reported **Mystic** to
`InvestigatorWithClass`, `DifferentClassAmong`, `CallForBackup2`, `RealityAcid` and
`SearchCollectionForRandom`. Fixed in the same shape as the stat fields
(`(toAttrs iinvestigator).classSymbol` — the `HasField` label is `classSymbol`, since
`class` is a Haskell keyword).

**Why:** the mechanism is opt-in per field, so any *new* printed-value field is wrong by
default and fails silently as a plausible number, never an error.

**How to apply:** when adding a field that reads a printed value off the investigator
card, add the `TransfiguredForm` branch at the same time. To verify one, patch the
export's form to an investigator of a class/stat the state does not otherwise contain
(`jq '.campaignData.currentData.gameEntities.investigators["<iid>"].form =
{"tag":"TransfiguredForm","contents":"c02001"}'`) and re-run `arkham-replay` — a single
post-fix number is then decisive without needing the pre-fix binary.

**The wire is a third place.** The raw attrs keep the original printed values so the
form can be dropped again, so `PublicGame` publishes the *original* class unless it is
overridden too. `Investigator.vue` already renders the transfigured card image
(`form.tag === "TransfiguredForm"` -> `cardImage(form.contents)`) but themes off
`investigator.class`, so the client showed Norman's front in Luke's colours. Fixed in
`ToJSON WithDeckSize` (`Game.hs`), the wire-only seam that already inserts `deckSize`;
every client consumer then follows with no frontend change. `With` and `WithDeckSize`
both define only `toJSON`, so `PublicGame`'s explicit `toEncoding` routes through it.
Covered by "publishes the transfigured class" in `Transfiguration2Spec`.

Related: [[project_transformed_investigator_identity]],
[[project_investigatorfromattrs_reseeds_metadata]].
