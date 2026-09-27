---
title: project-homebrew-extension-seams
description: Homebrew extension seams — how to add traits/actions/deck-keys/log-keys/tokens without editing core types; guide + skill locations
---

Reworked 2026-07-14: homebrew campaigns extend core sum types WITHOUT editing them. Guiding rule: **adding homebrew content never edits a core type**; every seam has a compile-time dup-guard (build fails if a homebrew name equals a core constructor = it was promoted, remove from homebrew list).

**Seams** (each core type has ONE open escape-hatch constructor; campaign declares named values via TH splice in a per-campaign file):
- **Traits** — `Arkham.Trait.HomebrewTrait Text`; `<Campaign>/Traits.hs` `declareHomebrewTraits [...]`; wire `hdTraits` in `Defs.hs`; full set = `Arkham.Homebrew.Defs.allTraits`.
- **Actions** — `Arkham.Action.HomebrewAction Text`; `<Campaign>/Actions.hs` `declareHomebrewActions [...]` + `actionAffordability :: [(Action, Criterion)]` (affordability is DATA a `Criterion`, evaluated generically in core `Helpers.Ability.canDoAction'`; no entry = always affordable); wire `hdActions`/`hdActionAffordability`; full set = `allActions`.
- **Scenario deck keys** — `Arkham.Scenario.Deck.HomebrewScenarioDeckKey Text`; `<Campaign>/ScenarioDeckKeys.hs` `declareHomebrewScenarioDeckKeys [...]`.
- **Campaign-log keys** — DIFFERENT: NOT a TH list, NO promotion. Campaign owns its own `data <Campaign>Key` enum (like `TheDunwichLegacyKey`) + `IsCampaignLogKey` instance mapping through the single shared `HomebrewCampaignLogKey Text` wrapper (`toCampaignLogKey = HomebrewCampaignLogKey . tshow`; `fromCampaignLogKey = readMay . unpack`). Needs `Read` derived; used bare (`record Memories`).
- **Custom tokens** — `CustomTokenDef` in `<Campaign>/Tokens.hs` (`IsHomebrewTokens`); frontend totals bar via `frontend/homebrew/<campaign>/tokens.json` `[{face, tooltip}]` (counted across chaos bag + investigators' sealed tokens; Scenario.vue `homebrewTotals`).

The shared TH is `Arkham.Homebrew.TH.declareOpenExtension` (Trait/Action/ScenarioDeckKey specializations). Open constructor breaks `Enum`/`Bounded` — use `allTraits`/`allActions`, never `[minBound..maxBound]`. Escape-hatch types hand-write ToJSON/FromJSON (tag==name, core-name lookup first). No legacy back-compat kept (no homebrew games exist yet) — removed EncounterSet `legacySlug` too.

`HomebrewDefs` (light, CardDef metadata + traits/actions) and `HomebrewContent` (heavy, `Some*Card` RunMessage impls) MUST stay separate — merging cycles (Defs is imported by low-level `Game`/`Helpers.Ability`/`*/Cards.hs`).

**Guide:** `docs/homebrew.md` (hands-on: seam syntax + campaign layout + id/image conventions + checklist; checked in, linked from root README). **Skill:** `/add-homebrew-content` (`.claude/commands/add-homebrew-content.md`). Serialization specs: `tests/Arkham/{Trait,Action,ScenarioDeckKey,CampaignLogKey}Spec.hs`. Related: [[project_homebrew_campaigns]].
