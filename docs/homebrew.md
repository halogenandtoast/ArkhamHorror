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
  (`:dark-matter:057b`), same as official cards. When the back is a card def of
  its own — an agenda that flips to an enemy, a location whose two faces are
  different places — point the back's `cdOtherSide` at the front. The card
  browser lists only the earlier side of a linked pair (unsuffixed before `a`
  before `b`), so without the link the same card shows up twice.
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
| `Defs.hs` | your campaign's printed-card manifest, plus your traits & actions |
| `Content.hs` | your campaign's in-play manifest, plus your scenarios & campaign |
| `CardDefEntries.hs`, `CardEntries.hs` | one-line generated files that find your cards for those two manifests |
| `Campaign.hs`, `CampaignSteps.hs` | your campaign log, interludes, and how scenarios branch |
| `Key.hs` | your campaign-log keys (the flags/tallies your campaign records) |
| `Traits.hs` | new traits your cards use |
| `Actions.hs` | new actions (like Dark Matter's "Scan") |
| `ScenarioDeckKeys.hs` | new named decks a scenario sets aside |
| `Tokens.hs` | custom chaos tokens |
| `AchievementDefs.hs`, `Achievements.hs` | your achievement list, and the code that notices when one is earned |
| `Sets.hs` | your encounter sets |
| `Helpers.hs`, `Import.hs`, `ChaosBag.hs` | campaign-specific helpers, shared import surface, chaos bag |
| `Scenarios/<Name>.hs` | your scenario runners |

`Defs.hs` and `Content.hs` are the two "manifest" files. `Defs.hs` covers the
*cards on paper*; `Content.hs` covers the *cards in play*. Keep them apart and
never import your card behaviors into `Defs.hs` — that's the only structural rule,
and following it keeps the build healthy. A homebrew *standalone* (a one-off
scenario with no campaign) uses the same folder shape, minus `Campaign.hs`.

Neither manifest lists your cards by hand. `CardDefEntries.hs` and
`CardEntries.hs` are one-line files (copy them from Dark Matter) whose pragma
scans your folder: every `name :: CardDef` under `CardDefs/` and every
`name :: EnemyCard Foo`-style builder in your behavior modules is registered
automatically, and each definition is sorted by its card type. So adding a card
means adding a card — nothing to register.

Because sorting goes by card type, the **card back matters** when you define a
story asset. Encounter back → `encounterAsset` / `encounterAsset_`; player back
(it goes in a deck or a hand) → `storyAsset` / `storyAsset_`. Get it wrong and
the card is dealt from the wrong side of the table — it shows the wrong back and
generates as the wrong kind of card.

```haskell
sophie :: CardDef            -- encounter back
sophie = encounterAsset_ ":dark-matter:135" "Sophie" Set.InTheShadowOfEarth

spaceArtillery :: CardDef    -- player back
spaceArtillery = storyAsset ":dark-matter:120" "Space Artillery" 4 Set.InTheShadowOfEarth
```

A card whose back is its own `b` side rather than a stock back — Dark Matter's
scanning backs, with their icons printed at the bottom — says so with
`cdOtherSide = Just (flippedCardCode def.cardCode)` (locations have
`singleSidedWithFlippedBack` for this). Without it the card falls back to the
generic encounter back and the icons never show.

An encounter-backed card can still be *earned*: `addCampaignCardToDeck` records
it in the campaign's story cards for its owner whichever side it is printed on.
It will not come back with the deck, though — deck loading keeps only player
cards — so only a `permanent` one survives the scenario it was earned in.
`SetupInvestigator` puts those into play from the campaign story cards in the
same step it plays the deck's own permanents (Dark Matter's Heir to Carcosa).
Non-permanent encounter-backed earned cards have nowhere to live; give those a
player back.

Story cards are the one case where card type can't record this (there is no
player-back story type), so a story printed on a player back — Dark Matter's
"Delights" — declares `delights :: PlayerCardDef` instead, importing
`PlayerCardDef` from `Arkham.Homebrew.DefsBase`. It's the same type; the
signature is what discovery reads.

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

Each key needs display text under `key.<camelCaseName>` in your `locales/en/base.json`
— `TheRingmasterHasHisEyeOnYou` reads `key.theRingmasterHasHisEyeOnYou`. Miss one and
the log shows the raw path instead of a sentence.

You may optionally namespace keys as `HomebrewCampaignLogKey . ("yourCampaign." <>) . tshow`
(Dark Matter does), which lets the frontend read the i18n scope straight off the key.
It is not required — the log falls back to the scope of the campaign it is rendering.
Do not add or remove that prefix mid-campaign: `hasRecord` compares the serialized key,
so anything already recorded in a save stops matching.

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

### Achievements

Printed an achievement list for your campaign? Two files. The list itself goes in
`AchievementDefs.hs` — a plain enum, in printed order, and nothing else:

```haskell
module Arkham.Homebrew.YourCampaign.AchievementDefs where

import Arkham.Homebrew.AchievementDefs
import Arkham.Prelude

achievementCampaign :: Text
achievementCampaign = ":your-campaign"

data YourCampaignAchievement
  = Scapegoat
  | ManyFutures
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

-- Items for achievements that are finished across several playthroughs; `[]` for
-- an ordinary one-shot earn.
achievementChecklistItems :: YourCampaignAchievement -> [Text]
achievementChecklistItems = \case
  ManyFutures -> ["OracleOfPurity", "OracleOfMystery"]
  _ -> []

data YourCampaignAchievements

instance IsHomebrewAchievements YourCampaignAchievements where
  homebrewAchievements =
    campaignAchievements achievementCampaign $ map def [minBound .. maxBound]
   where
    def a = case achievementChecklistItems a of
      [] -> achievement (tshow a)
      items -> checklistAchievement (tshow a) items
```

That file is deliberately a leaf — it imports nothing from the engine — because
the base game reads your list out of it to build the achievement catalog. Keep
the detection out of it.

The detection goes in `Achievements.hs`, hooked into your campaign's own
`runMessage`, which sees every message in the game before anything else does:

```haskell
runMessage msg c =
  runQueueT $ campaignI18n $ lift (runYourCampaignAchievements msg) *> case msg of
```

```haskell
earn :: (HasGame m, HasQueue Message m) => YourCampaignAchievement -> m ()
earn = earnAchievement . homebrewAchievement achievementCampaign . tshow
```

`earnAchievement` already checks that achievements are on for this game and that
the campaign is yours, and the server ignores an earn it has already recorded, so
a condition that re-checks itself is fine. Cross-playthrough items are reported
with `achievementProgress` instead; the server collects them per player and
awards the achievement once every box is checked. Study
`Arkham/Homebrew/CircusExMortis/Achievements.hs` — it is the worked example, and
its header lists the timing traps (never key on `ScenarioResolution`; key on what
a resolution *records*).

On the frontend, `frontend/homebrew/<campaign>/achievements.json` lists the same
keys in printed order and is discovered like every other homebrew file:

```json
{
  "campaign": ":your-campaign",
  "entries": [
    { "key": "Scapegoat" },
    { "key": "ManyFutures", "items": ["OracleOfPurity", "OracleOfMystery"] }
  ]
}
```

Names and descriptions live in your own locale folder, in
`locales/en/achievements.json` under an `achievements` key —
`achievements.Scapegoat.name` / `.text`, and `.items.<key>` for a checklist's
boxes. Write token names as words ("moon tokens"), not `{moon}`: achievement text
is rendered as plain strings. Nothing else is needed — the new-game toggle, the
campaign log's Achievements tab, the /achievements page and the unlock toast all
read the catalog.

### Drawing one of your own questions

A question your campaign asks renders through `StoryQuestion`, which draws card
choices as a small row of images marked `no-overlay` — they cannot be zoomed. When
that is the wrong shape (a shop, a board, anything where the player has to *read*
the cards), draw the question yourself: drop a component at

```
frontend/homebrew/<campaign>/question-panels/<label path>.vue
```

named after the question's label with its campaign scope and `label.` prefix
stripped — `questionLabeled "scienceExpansion.purchase"` under `campaignI18n`
builds `$darkMatter.label.scienceExpansion.purchase`, so the file is
`question-panels/scienceExpansion.purchase.vue`. Discovered like your locales and
`campaign.json`; nothing is registered centrally.

The component is handed `{ game, playerId, viewOnly }` and emits `choose(index)`
against the question's own choices, exactly like a log panel:

```vue
const props = defineProps<{ game: Game; playerId: string; viewOnly?: boolean }>()
const emit = defineEmits<{ choose: [value: number] }>()
```

Read the choices off `game.question[playerId]` (unwrap `QuestionLabel` to its
`question`), pick out the `CardLabel`s by index, and emit the index the player
clicked. The panel owns its whole layout and styles, so size the cards however
your content needs — and leave `no-overlay` off the images if you want the normal
hover zoom as well.

### Extra actions on the continuation screen

The screen between scenarios shows Continue, Upgrade Decks and Add Side Scenario.
A campaign can add buttons of its own to it — Dark Matter's Science Expansion
sells its "Researched" story assets there — by answering
`campaignContinueOptions`:

```haskell
instance IsCampaign YourCampaign where
  campaignContinueOptions (YourCampaign attrs) =
    [ ContinueOption
        { key = "yourCampaign.theShop"
        , label = "yourCampaign.theShop.button" -- a full i18n key
        , available = somethingAboutAttrs
        }
    ]
```

Only `available` options are drawn. Pressing one answers with
`CampaignOptionStep <key> <a continuation that redraws this screen>` — a whole
`ContinueCampaignStep`, not the bare next step, or handing it back would start the
next scenario instead. You handle it like any other step and hand the table back
when you are done:

```haskell
    CampaignStep (CampaignOptionStep k ret) | k == "yourCampaign.theShop" -> do
      ...                   -- your prompts
      push $ SetCampaignStep ret
      push $ CampaignStep ret
      pure c
```

`label` is a whole i18n key rather than a scoped fragment, because your campaign
owns its own locale namespace (`yourCampaign.*`).

One gotcha that is not about this seam: a campaign option chosen at creation time
(`frontend/homebrew/<campaign>/campaign.json`'s `recommendedOptions`) only
reaches the campaign log if the campaign *handles* it —
`HandleOption opt -> pure $ YourCampaign $ c.attrs & logL . optionsL %~ insertSet opt`.
There is no generic handler; an unhandled option is silently dropped.

## The frontend side

Your campaign's art, text, and player-facing config live in
`frontend/homebrew/<your-campaign>/` (kebab-case, the campaign id without its
leading colon), discovered the same hands-off way — no registration anywhere:

| File | What it's for |
|------|---------------|
| `campaign.json` | your campaign's new-game entry — name, `designer`, `chapter`, difficulty chaos bags. Appears in a dedicated **Homebrew** section of the new-game screen with a "designed by …" credit. |
| `scenarios.json` | the scenario list; each entry's `i18n` key names its locale scope |
| `icons.json` | custom icon names, e.g. `{"moon": "moon-icon"}` — hooks `{moon}` into flavor text and `[moon]` into card text. Style the class in `style.css`; if the icon is art rather than a font glyph, `mask` the image and paint it with `background-color: currentColor` so it follows the surrounding text color (button labels are light-on-dark) |
| `tokens.json` | custom tokens to show in the scenario **totals bar** and in the chaos-bag debug panel, e.g. `[{ "face": ":your-campaign:moon", "tooltip": "Moon Tokens", "icon": "moon-icon", "background": "#ffffff", "iconColor": "#2D3F4E" }]` (counted across the chaos bag and players' sealed tokens) |
| `style.css` | your campaign's styling (use absolute `/img/arkham/homebrew/<campaign>/…` urls inside) |
| `fonts.json` | fonts your text uses, family name → file, e.g. `{"Corvisa": "fonts/corvisa.ttf"}`, or an object to give a face its own settings: `size` (`"2em"`) and `stroke` (`"0.15px"`, a hairline for a font that ships only one weight). Each entry gets an `@font-face` and a `.font-<slug>` class (`Corvisa` → `font-corvisa`), so flavor text can say `<div class='font-corvisa'>…</div>`. Drop the file in the campaign folder; Vite bundles it, so there is nothing to sync. |
| `locales/en/*.json` | your text — `base.json`, `interludes.json`, one file per scenario; merged under the campaign's message scope, with English fallback for other languages |
| `img/` | art: `cards/`, `boxes/`, `chaos-tokens/`, `icons/`, `encounter-sets/`. Synced to the asset host by `make sync-images`; in dev a Vite middleware serves them straight from this folder, so a local/empty asset host works without syncing. |

The directory name camelCases to the i18n message scope (`circus-ex-mortis` →
`circusExMortis`), which matches the backend's campaign i18n scope.

`campaign.json` also declares which rules chapter your campaign is written
against: `"chapter": 1` or `"chapter": 2`. Official campaigns derive this from
their id (`11` and up are Chapter 2), but homebrew ids don't order that way, so
say it outright. It preselects the Chapter 1/Chapter 2 rules toggle (currently
the "as if" ruling) on the new-game screen; players can still override it there
and in game settings. Omit it and you get Chapter 1.

`fonts.json` covers the whole job of using your own typeface: the family is
registered and a class is generated, so you never write `@font-face` yourself.
The class works on a wrapper as well as a single element, because the generated
rule reaches paragraphs inside it — flavor text styles every `<p>` it renders,
which would otherwise beat a family inherited from an ancestor. A
`<p class='basic'>` inside keeps the plain UI font, since `basic` means "not
flavor text"; so does a `_bold_` run. Families are shared across campaigns, so
name yours after the typeface and expect the last one registered to win a clash.
The class works inline too, so one word can be set in a different hand — the
signature on Erich Zann's opening letter is a `<span class='font-vivaldi'>`
inside a paragraph the rest of the letter sets in Corvisa. Give a display script
a `size` and it also gets `line-height: 1`, so an inline run at `2em` does not
crash into the line above it.

`stroke` is there because `font-weight` cannot help a single-weight face: the
browser's synthetic bold is all-or-nothing and smears a script badly. A hairline
in `currentColor` thickens it by as little as you like. A family that really
does ship a bold should register that file as its own entry instead. Because
`-webkit-text-stroke` inherits, every generated class states its own width — `0`
when none was asked for — so a face nested inside a stroked one does not wear
its parent's stroke, and `.basic` opts out the same way it opts out of the font.

Watch out for a font file that *fetches* fine and still never appears: Chrome
runs every webfont through the OpenType Sanitizer, and a rejection shows up only
as a console warning (`Failed to decode downloaded font`, then `OTS parsing
error: …`). Old conversions often trip it — this campaign's Vivaldi declared
`language=1` on its `cmap` subtables where the sanitizer demands 0, which
`fontTools` resets in a few lines.

`tokens.json` is a nice small example of a self-configuring feature: list a token
face there and it appears in the on-screen totals and in the chaos-bag debug
panel (as a −/icon/+ button that adds and removes it) with no code changes. Only
*your* campaign's games offer it: a token belongs to the campaign named in its
slug. `icon` names a class your `style.css` defines — a masked glyph painted with
`currentColor`, like the one `icons.json` hooks into card text — and
`background`/`iconColor` are your control over how the button reads; `iconColor`
paints the glyph *and* the −/+ labels, so pick a pair that contrasts. Leave `icon`
out and the debug button falls back to the token art.

## Getting started

1. Make `Arkham/Homebrew/YourCampaign/` with a `Defs.hs`, `Content.hs`,
   `CardDefEntries.hs`, and `CardEntries.hs`. Copy the shape from an existing
   campaign — the manifests are short, and the two entry files are one line each.
2. Add cards under `CardDefs/` (the printed side) and the matching behavior
   modules. They're discovered; there's no list to update.
3. Add `Key.hs`, `Sets.hs`, and any of `Traits.hs` / `Actions.hs` /
   `ScenarioDeckKeys.hs` / `Tokens.hs` you need. Wire the trait/action lists into
   `Defs.hs`.
4. Write your campaign log and scenarios (`Campaign.hs`, `CampaignSteps.hs`, your
   scenario modules).
5. Add `frontend/homebrew/your-campaign/` with at least `campaign.json` and
   `scenarios.json`.
6. Build. Your content shows up on its own.

If something you added doesn't appear, it's almost always because a manifest file
(`Defs.hs`, `Content.hs`, `Tokens.hs`) is missing its instance or is misnamed, or
because a card's signature isn't the one-line `name :: CardDef` the scan reads —
the game finds your content by those files, so the names have to match. When in
doubt, diff your folder against Dark Matter or Circus Ex Mortis.

## Campaigns in the repo

| Campaign | Id | Designer | Notes |
|---|---|---|---|
| Dark Matter | `:dark-matter` | Axolotl | based on "Ripples from Carcosa" (Oscar Rios) and "The End Time" (Michael C. LaBossiere); requires The Path to Carcosa collection |
| Circus Ex Mortis | `:circus-ex-mortis` | Tyler Gotch (moon token design: Hauke) | original fan campaign |
