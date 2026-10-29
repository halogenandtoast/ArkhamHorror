# Night of Vespers

Development version of Alex / Exhaled Innards’ campaign. Campaign progression,
prologue, interludes, scenario introductions, rules and epilogue are implemented.
Both the campaign and all eight scenarios remain `dev: true` while scenario
boards, encounter cards and resolutions are developed. Five interlude reward
cards are registered; all other card behavior remains deferred.

## Locations

- Backend: `backend/arkham-api/library/Arkham/Homebrew/NightOfVespers/`
- `campaign.json`: discovery metadata and the four printed starting chaos bags.
- `scenarios.json`: eight scenario reference IDs and locale scopes.
- `img/cards/`: AVIF card faces, served automatically for `:night-of-vespers:NNN` codes.
- `img/sets/`: campaign/encounter icons and numeric scenario-reference aliases.
- `img/boxes/`: campaign and scenario art cropped from the supplied guide.
- `sources/project.json`: unmodified arkham.build export, including full card text,
  printed stats, source UUIDs, credits and image URLs.
- `sources/card-manifest.json`: stable source UUID → engine code → local image map.
- `sources/campaign-guide.ocr.txt`: searchable OCR of all 51 PDF pages. This is
  machine transcription, not authoritative text; proofread against the PDF.
- `sources/guide-index.json`: scenario, interlude and reference page index.
- `sources/artwork-crops.json`: guide page and fractional rectangle for each crop.

## Sources

Preview: https://arkham.build/fan-made-content/preview/f565e2b2-97df-420c-ae07-7586fb74c7d7

Export: https://cdn.arkham.build/fan_made_content/f565e2b2-97df-420c-ae07-7586fb74c7d7/project.json

Designer: https://mysteriouschanting.wordpress.com/2024/12/17/night-of-vespers/

Guide supplied locally as `~/Downloads/Night of Vespers Campaign Guide.pdf`
(51 pages, updated February 2026). The original is about 1 GB and is not copied
into the repository. The export is dated January 2026; check the guide for any
later changes when implementing rules. Art credits are retained in the export
and in guide OCR pages 50–51. The supplied guide’s cover is by Vanessa Lemen;
the export’s banner is by Betty Jiang.

## Stable numbering

Use the export’s unique printed `position`, zero-padded to three digits, for
`:night-of-vespers:001` through `:night-of-vespers:383`. A linked back uses the
same position with a `b` suffix. Do not renumber when adding implementations.
There are 383 front records, 206 linked backs and one additional image-only
back for Shattered Reflections (`121b`). That back is provided by
`back_image_url`, not a separate hidden card record. Its front must eventually
link to the flipped back so the engine displays that art.

The expansion icon is normalized to a black foreground with its transparency
preserved, matching the app’s existing icon filters.

The source has 32 encounter sets. All have namespaced patterns in `Sets.hs`
and matching images in `img/sets/`. Official encounter sets, including the
Core Set’s Chilling Cold, must be reused rather than duplicated.

## Campaign structure to implement

| Guide label | Scenario | Reference code | PDF page |
| --- | --- | --- | --- |
| 1 | The End of Seasons | `:night-of-vespers:001` | 3 |
| 2A | Behind Closed Doors | `:night-of-vespers:037` | 8 |
| 2B | Dancing with Death | `:night-of-vespers:075` | 13 |
| 3 | The Teachings of Aradia | `:night-of-vespers:105` | 18 |
| 4A | The City of Fog | `:night-of-vespers:136` | 24 |
| 4B | The Garden of Bones | `:night-of-vespers:168` | 29 |
| 4C | A Chorus of Suffering | `:night-of-vespers:214` | 33 |
| 5 | Mother’s Mercy | `:night-of-vespers:257` | 40 |

These are branching scenario labels, not an eight-scenario linear sequence.
`CampaignSteps.hs` reserves names for all eight scenarios and four interludes;
the campaign follows their printed branch decisions and interlude effects. The guide also
allows standalone play, which remains disabled until standalone setup exists.

Page 2 defines Jinx/Jinxed: a failed fight, evade or investigate test at a
location with a Jinxed card resolves the current act/agenda’s printed Jinx
Forced effect. Multiple Jinxed cards do not stack unless specified; without a
defined Jinx effect they do nothing. This shared mechanic is implemented; see [JINX.md](JINX.md).
Page 50 lists achievements; they are not registered until detection exists.

## Continuing implementation

Follow `docs/homebrew.md`. Add printed definitions under `CardDefs/` and matching
behavior modules in this campaign folder; the two entry modules discover them.
Preserve printed abilities and default entity abilities as required by AGENTS.md.
Keep `dev: true` until the campaign is playable. Local assets use the existing
Vite homebrew middleware; normal `make sync-images` handles later publication.
No assets or custom-card definitions have been uploaded to production.

## Campaign layer implemented

The prologue, rules copy, four interludes, all eight scenarios' conditional
introductions, travel letters and conditional epilogue are localized and wired.
Both chapter II scenarios are played in companion-dependent order. All three
chapter IV destinations are visited in the investigators' chosen order.
Campaign keys are namespaced under `nightOfVespers.`; the Name of the Rose is
an investigator-specific record and awards only its chosen reader 2 XP.

The five cards awarded directly by interludes are implemented: Finlay O'Doyle
(`032`), Gigi Giuliano (`033`), and the three Favor weaknesses (`124`–`126`).
Research draws stop at the first specified symbol or at bag exhaustion; if no
specified symbol appears, no Favor is awarded. The captive removed in Interlude
IV is retained with its owner in campaign storage for Mother's Mercy setup.

Scenario introductions remember selected leads and the Garden of Bones starting
route for their future board setup. Their board-dependent instructions are shown
as guide text; board creation, setup effects involving unimplemented encounter
cards, scenario resolutions, and the remaining cards are still deferred. The
campaign remains development-only.

See [JINX.md](JINX.md) for the shared keyword mechanics and source-wide audit.
