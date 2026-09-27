---
name: project_dream_eaters_split_campaign_log
description: "The Dream-Eaters ships the whole inactive campaign in campaignMeta.otherCampaignAttrs; anything the campaign log shows must switch on the selected side, not just the notes"
metadata: 
  node_type: memory
  type: project
  originSessionId: 9b1cdf5f-c5e5-466a-bf34-6af1f23d3667
  modified: 2026-08-05T07:30:44.587Z
---

The Dream-Eaters (campaign `06`) runs The Dream-Quest and The Web of Dreams in parallel.
The active side is the live `CampaignAttrs`; the inactive side rides along whole inside
`campaignMeta` (`Arkham/Campaigns/TheDreamEaters/Meta.hs`):

- `otherCampaignAttrs` — the other side's **log, chaosBag, xpBreakdown, decks, storyCards**
- `otherCampaignPlayers :: Map PlayerId InvestigatorAttrs` — the other side's investigators
  **with their xp and trauma**. Only populated once the campaign has swapped sides at least
  once (`TheDreamEaters.hs` `setCampaignPart` / the `ContinueCampaignStep` branches), so
  code reading it needs a fallback to the `otherCampaignAttrs.decks` key list for deck
  selection and pre-swap states.

`CampaignLog.vue` has a radio that picks the side. #5338: it switched only the notes —
chaos bag and xp breakdown were hard-wired to `game.campaign`, and the selector was buried
inside the Log tab so the Investigators tab looked frozen. Fixed with a single `showingMain`
computed driving `investigators` / `chaosBag` / `breakdowns`, `otherXpBreakdown` +
`otherChaosBag` refs decoded from the meta the same way `otherLog` already was,
`allGameInvestigators` widened to include `game.otherInvestigators` (so the other side's
xp-breakdown rows resolve), and the radio moved **above** the tab nav since it changes what
every tab describes.

**Why:** `campaignMeta` is an untyped `Value` on the frontend (`types/Campaign.ts` has
`meta: JsonDecoder.succeed()`), so nothing type-checks that a new campaign-log panel handles
the split — each one has to opt in by hand.

**How to apply:** any new panel added to the campaign log must ask "does this differ per
side?" and read `otherCampaignAttrs` when `!showingMain`. Decode with the real decoder
(`logContentsDecoder`, `xpBreakdownDecoder`, `tokenFaceDecoder`) rather than casting.

Unrelated pre-existing quirk found while verifying: `views/CampaignLog.vue` calls
`refreshGame()` once at setup, so hash-navigating between two `/games/<id>/log` routes
reuses the component and never refetches — the second game shows the first game's data
until a hard reload.

Related: [[project_publicgame_toencoding_is_the_wire]]
