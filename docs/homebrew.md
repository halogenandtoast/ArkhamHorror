# Homebrew (Fan-Made) Content Framework

This document defines how fan-made campaigns are integrated into the engine. Homebrew
content is fully compiled-in like official content, but lives in a distinct id namespace,
its own image directory, and a separate "Homebrew" section of the new-game screen that
credits the campaign designer.

## Id namespace

- Every homebrew card code is prefixed with `z-` followed by the campaign slug and a
  3-digit number: `z-dark-matter-001`, `z-circus-ex-mortis-042`.
- Numbers are assigned in pack `position` order and are stable — never renumber.
- The `z` prefix guarantees no collision with official ArkhamDB codes (numeric) and is
  excluded from Chapter 2 detection (`isChapterTwo` checks `12*`/`60x5-9` prefixes).
- Card codes ending in digits never trip the `CardCode` loose-`Eq` side-suffix logic
  (`a`–`d` trailing letters); double-sided homebrew cards use the standard `b` suffix
  (`z-dark-matter-057b`), same as official cards.
- Campaign ids are the slug with the same prefix: `z-dark-matter`, `z-circus-ex-mortis`.
- Scenario ids are the z-code of the scenario reference card.

## Images

Official card images live at `<assetHost>/img/arkham/cards/<code>.avif`. Homebrew card
images live in a **sibling** directory:

```
<assetHost>/img/arkham/homebrew/<z-code>.avif        # front
<assetHost>/img/arkham/homebrew/<z-code>b.avif       # back, when double-sided
<assetHost>/img/arkham/homebrew/boxes/<campaign-id>.jpg
```

`frontend/src/arkham/cardImages.ts` routes any code starting with `z-` to `homebrew/`
instead of `cards/`. An image manifest (z-code → source URL) for each campaign is kept
under `docs/homebrew/` so art can be fetched/copied into the asset host.

## Backend layout (per campaign, mirroring official conventions)

- `Arkham/Campaign/Campaigns/<Name>.hs` — campaign runner (auto-discovered), registered
  by hand in `Arkham.Campaign.allCampaigns` under the `z-<slug>` id.
- `Arkham/Campaigns/<Name>/` — `CampaignSteps.hs`, `Key.hs` (campaign log keys),
  `Helpers.hs`, `ChaosBag.hs`, plus any campaign-wide custom mechanics.
- `Arkham/Scenario/Scenarios/<ScenarioName>.hs` — one per scenario (auto-discovered),
  registered by hand in `Arkham.Scenario.allScenarios` and `scenarioEncounterSets`.
- CardDefs per type follow the type's current convention
  (`Arkham/<Type>/CardDefs/<Name>.hs` or `Arkham/<Type>/Cards/<Name>.hs`) and every def
  is added to that type's aggregate list in `Arkham/<Type>/Cards.hs`.
- Entity implementations live in the normal per-type dirs. **All homebrew definition
  names and entity module names carry the campaign name as a suffix**
  (`anachronismDarkMatter` / `AnachronismDarkMatter.hs`) to rule out collisions with
  official cards and other homebrew sets — the same pattern official reprints use
  (`easttownTheDrownedCity`).
- New encounter sets get constructors in `Arkham.EncounterSet` prefixed with the
  campaign name (e.g. `DarkMatterAnachronism`). When a homebrew campaign reuses an
  official encounter set (e.g. Circus Ex Mortis gathers *The Bayou* and
  *Curse of the Rougarou*), reference the official set and cards — do not duplicate them.
- New traits introduced by fan cards are added to `Arkham.Trait` normally.

## Frontend

- `frontend/src/arkham/data/campaigns.json`: homebrew entries carry
  `"homebrew": true` and `"designer": "<name>"` (plus the usual difficulty chaos bags).
- `frontend/src/arkham/data/<campaign>.json` + registration in `data/scenarios.ts`.
- `NewCampaign/ChooseMode.vue` renders a third, separate **Homebrew** section beneath
  the official chapters. Each homebrew box shows the campaign name and a
  "designed by <designer>" credit line; a styled fallback tile is used when box art is
  missing.
- i18n: `frontend/src/locales/en/<campaign>/` (base.json, one file per scenario,
  interludes.json), registered in each `locales/<lang>.ts` (non-English languages fall
  back to English).

## Ground rules

- Homebrew never modifies official card behavior; shared engine changes must be
  campaign-agnostic.
- Campaign guides + normal Arkham rules govern ambiguity; the designer's wording wins
  for card text, standard timing/rules win where the fan wording is loose.
- Fan player-card pools are out of scope unless a campaign hands specific story
  assets/weaknesses to a deck (those are registered as normal story assets).

## Current homebrew campaigns

| Campaign | Id | Designer | Source |
|---|---|---|---|
| Dark Matter | `z-dark-matter` | Axolotl | fan campaign, based on "Ripples from Carcosa" (Oscar Rios) and "The End Time" (Michael C. LaBossiere); requires The Path to Carcosa collection |
| Circus Ex Mortis | `z-circus-ex-mortis` | Tyler Gotch (moon token design: Hauke) | original fan campaign |
