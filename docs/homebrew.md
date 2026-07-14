# Writing homebrew content

Want to build your own campaign, scenario, or one-off content for this engine?
This is the place. Homebrew content lives in its own folder and plugs itself in —
**you never touch the base game's files to add it.**

- Backend (game logic): `Arkham/Homebrew/<YourCampaign>/`
- Frontend (art, text, config): `frontend/homebrew/<your-campaign>/`

The two campaigns already in the repo — **Dark Matter** and **Circus Ex
Mortis** — are your best reference. Open them side by side with this guide and
copy what you need.

## The mental model

Think of a homebrew campaign as a self-contained mod. Everything it adds — cards,
enemies, locations, a chaos-bag, custom tokens, even new *traits* and *actions* —
goes in your folder. When the game builds, it finds your folder and folds your
content into the base game automatically. You don't register anything in a
central list, and you don't edit the core types.

There is one thing worth knowing up front: the base game defines a **closed set**
of things like traits (`Ally`, `Woods`, …), actions (`Fight`, `Investigate`, …),
and so on. You can't literally add a new value to those base lists from your
folder. Instead, each of those types has a small **open door** built into it, and
you declare your own values through that door. It reads exactly like using a
built-in value — the door is invisible at the usage site. A lot of this guide is
"here's the door for X, here's how you go through it."

> **The golden rule.** If you're editing a file under `Arkham/` that isn't inside
> your `Homebrew/<Campaign>/` folder, you're doing it wrong — there's a door for
> what you want. The one thing the build enforces: if you name a homebrew value
> the same as a real base-game value, it won't compile. That's on purpose (it
> means your value "graduated" into the base game and you should delete your copy).

Two ground rules keep homebrew from destabilizing the base game:

- **Homebrew never changes official card behavior.** If you need a shared engine
  change, make it campaign-agnostic so official content is unaffected.
- **The designer's wording wins for card text; standard rules win where the fan
  wording is loose.** Campaign guides plus normal Arkham rules govern ambiguity.

## Ids, card codes, and images

Homebrew uses its own id namespace so it never collides with official content.

- **Campaign id**: the slug with a leading colon — `:dark-matter`,
  `:circus-ex-mortis`.
- **Card code**: `:<campaign-id>:<number>` — `:dark-matter:001`,
  `:circus-ex-mortis:042`. Numbers are assigned in pack order and are **stable —
  never renumber**. Double-sided cards use a `b` suffix on the back
  (`:dark-matter:057b`), same as official cards.
- **Scenario id**: the card code of the scenario reference card.

Card art lives under your frontend folder and is served at
`homebrew/<campaign>/cards/<number>.avif` (and `<number>b.avif` for backs) — the
frontend routes any `:campaign:number` code there automatically, so you don't
wire up image paths.

## What's in a campaign folder

You don't need all of these — add a file only when you have something to put in
it. A minimal campaign is really just cards plus a couple of list files.

| File | What it's for |
|------|---------------|
| `CardDefs/`, `Locations/`, `Enemies/`, `Assets/`, … | your cards: the printed stats (`CardDefs/`) and the behavior (`Locations/`, `Enemies/`, …) |
| `Defs.hs` | the list of your cards' printed definitions, plus your traits & actions |
| `Content.hs` | the list of your cards' behaviors, plus your scenarios & campaign |
| `Campaign.hs`, `CampaignSteps.hs` | your campaign log, interludes, and how scenarios branch |
| `Key.hs` | your campaign-log keys (the flags/tallies your campaign records) |
| `Traits.hs` | new traits your cards use |
| `Actions.hs` | new actions (like Dark Matter's "Scan") |
| `ScenarioDeckKeys.hs` | new named decks a scenario sets aside |
| `Tokens.hs` | custom chaos tokens |
| `Sets.hs` | your encounter sets |
| `Helpers.hs`, `Import.hs`, `ChaosBag.hs` | campaign-specific helpers, shared import surface, chaos bag |
| `Scenarios/<Name>.hs` | your scenario runners |

`Defs.hs` and `Content.hs` are the two "manifest" files. `Defs.hs` lists the
*cards on paper*; `Content.hs` lists the *cards in play*. Keep them apart and
never import your card behaviors into `Defs.hs` — that's the only structural rule,
and following it keeps the build healthy. A homebrew *standalone* (a one-off
scenario with no campaign) uses the same folder shape, minus `Campaign.hs`.

## Adding things

Everything below is a door. Pick the one that matches what you want, and mirror
the shape from Dark Matter or Circus Ex Mortis.

### A new trait

Say your locations need a `NewMoonCircus` trait. List your traits in
`Traits.hs`:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Homebrew.YourCampaign.Traits (module Arkham.Homebrew.YourCampaign.Traits) where

import Arkham.Homebrew.TH (declareHomebrewTraits)

declareHomebrewTraits ["NewMoonCircus", "Tainted", "CircusTrain"]
```

That line writes each name as a real, usable trait. In `Defs.hs`, point your
campaign at the list: `hdTraits = Traits.traits`. Now your cards use it exactly
like a base trait: `[NewMoonCircus, Woods]`.

(If a trait name clashes with a location symbol — `Moon` is both a trait and a
board symbol — import your traits `hiding (pattern Moon)` so the symbol wins, and
that trait just stays a symbol.)

### A new action

Dark Matter adds a "Scan" action. Two parts: the action itself, and *when a
player is allowed to take it*. In `Actions.hs`:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Homebrew.YourCampaign.Actions (module Arkham.Homebrew.YourCampaign.Actions) where

import Arkham.Action (Action)
import Arkham.Criteria (Criterion (ScenarioDeckWithCard))
import Arkham.Homebrew.YourCampaign.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Homebrew.TH (declareHomebrewActions)

declareHomebrewActions ["Scan"]

-- "You can only Scan while the scanning deck has cards."
actionAffordability :: [(Action, Criterion)]
actionAffordability = [(Scan, ScenarioDeckWithCard ScanningDeck)]
```

Wire both into `Defs.hs`: `hdActions = Actions.actions` and
`hdActionAffordability = Actions.actionAffordability`. The "when can I take it"
part is just a `Criterion` — the same building block cards use for their own
conditions — so you describe the rule declaratively instead of writing engine
code. Leave it out and the action is always available.

### A named scenario deck

If a scenario sets a deck aside (like the scanning deck), name it in
`ScenarioDeckKeys.hs`:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Arkham.Homebrew.YourCampaign.ScenarioDeckKeys (module Arkham.Homebrew.YourCampaign.ScenarioDeckKeys) where

import Arkham.Homebrew.TH (declareHomebrewScenarioDeckKeys)

declareHomebrewScenarioDeckKeys ["ScanningDeck"]
```

Now use `ScanningDeck` anywhere the engine wants a deck key. Nothing else to wire.

### Encounter sets

Encounter sets go through the same kind of door in `Sets.hs`:

```haskell
pattern Anachronism :: EncounterSet
pattern Anachronism = Homebrew ":dark-matter:anachronism"
```

`Sets.hs` re-exports the base `Arkham.EncounterSet`, so one qualified import
covers both official and homebrew sets. If your campaign **reuses an official
encounter set** (Circus Ex Mortis gathers *The Bayou* and *Curse of the
Rougarou*), reference the official set and its cards directly — don't duplicate
them into your namespace.

### Campaign-log keys (what your campaign remembers)

These are the flags and tallies your campaign records — "the ringmaster has his
eye on you," a running count of "Memories," and so on. Unlike the seams above,
here you write a normal list of your own, in `Key.hs`, exactly like the base
campaigns do:

```haskell
module Arkham.Homebrew.YourCampaign.Key (module Arkham.Homebrew.YourCampaign.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data YourCampaignKey
  = TheRingmasterHasHisEyeOnYou
  | ImpendingDoom
  | Memories
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey YourCampaignKey where
  toCampaignLogKey = HomebrewCampaignLogKey . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMay (unpack t)
    _ -> Nothing
```

That little instance is the plug — copy it verbatim, changing only the type name.
Then your campaign code records and reads keys by name, like `record Memories` or
`getRecordCount ImpendingDoom`. (Keep the `Read` in the deriving list; the plug
uses it.)

### Custom chaos tokens

Add a token in `Tokens.hs` — a slug and what happens when it's revealed:

```haskell
module Arkham.Homebrew.YourCampaign.Tokens where

import Arkham.ChaosToken.Types
import Arkham.Homebrew.TokenDefs

pattern MoonToken :: ChaosTokenFace
pattern MoonToken = CustomToken ":your-campaign:moon"

customTokens :: [CustomTokenDef]
customTokens = [CustomTokenDef ":your-campaign:moon" SealOnRevealerAndRevealAnother]

data YourCampaignTokens
instance IsHomebrewTokens YourCampaignTokens where homebrewTokens = customTokens
```

The slug is `":<campaign-id>:<name>"`; the last part (`moon`) is the display and
icon name (icon art at `img/chaos-tokens/<name>.png` in your frontend folder).
The reveal effect is one of a few presets: do nothing (`RevealNoEffect`), reveal
another (`RevealAnother`), or seal-and-reveal-another
(`SealOnRevealerAndRevealAnother`). The engine only applies these during skill
tests, so custom tokens are inert outside them; anything richer, your scenario
handles in its own message code.

## The frontend side

Your campaign's art, text, and player-facing config live in
`frontend/homebrew/<your-campaign>/` (kebab-case, the campaign id without its
leading colon), discovered the same hands-off way — no registration anywhere:

| File | What it's for |
|------|---------------|
| `campaign.json` | your campaign's new-game entry — name, `designer`, difficulty chaos bags. Appears in a dedicated **Homebrew** section of the new-game screen with a "designed by …" credit. |
| `scenarios.json` | the scenario list; each entry's `i18n` key names its locale scope |
| `icons.json` | custom icon names, e.g. `{"moon": "moon-icon"}` — hooks `{moon}` into flavor text and `[moon]` into card text |
| `tokens.json` | custom tokens to show in the scenario **totals bar**, e.g. `[{ "face": ":your-campaign:moon", "tooltip": "Moon Tokens" }]` (counted across the chaos bag and players' sealed tokens) |
| `style.css` | your campaign's styling (use absolute `/img/arkham/homebrew/<campaign>/…` urls inside) |
| `locales/en/*.json` | your text — `base.json`, `interludes.json`, one file per scenario; merged under the campaign's message scope, with English fallback for other languages |
| `img/` | art: `cards/`, `boxes/`, `chaos-tokens/`, `icons/`, `encounter-sets/`. Synced to the asset host by `make sync-images`; in dev a Vite middleware serves them straight from this folder, so a local/empty asset host works without syncing. |

The directory name camelCases to the i18n message scope (`circus-ex-mortis` →
`circusExMortis`), which matches the backend's campaign i18n scope.

`tokens.json` is a nice small example of a self-configuring feature: list a token
face there and it appears in the on-screen totals with no code changes.

## Getting started

1. Make `Arkham/Homebrew/YourCampaign/` with a `Defs.hs` and `Content.hs`. Copy
   the shape from an existing campaign — they're a lightweight list of everything
   your campaign contributes.
2. Add cards under `CardDefs/` (the printed side) and the matching behavior
   modules; list them in `Defs.hs` / `Content.hs`.
3. Add `Key.hs`, `Sets.hs`, and any of `Traits.hs` / `Actions.hs` /
   `ScenarioDeckKeys.hs` / `Tokens.hs` you need. Wire the trait/action lists into
   `Defs.hs`.
4. Write your campaign log and scenarios (`Campaign.hs`, `CampaignSteps.hs`, your
   scenario modules).
5. Add `frontend/homebrew/your-campaign/` with at least `campaign.json` and
   `scenarios.json`.
6. Build. Your content shows up on its own.

If something you added doesn't appear, it's almost always because a manifest file
(`Defs.hs`, `Content.hs`, `Tokens.hs`) is missing its instance or is misnamed —
the game finds your content by those files, so the names have to match. When in
doubt, diff your folder against Dark Matter or Circus Ex Mortis.

## Campaigns in the repo

| Campaign | Id | Designer | Notes |
|---|---|---|---|
| Dark Matter | `:dark-matter` | Axolotl | based on "Ripples from Carcosa" (Oscar Rios) and "The End Time" (Michael C. LaBossiere); requires The Path to Carcosa collection |
| Circus Ex Mortis | `:circus-ex-mortis` | Tyler Gotch (moon token design: Hauke) | original fan campaign |
