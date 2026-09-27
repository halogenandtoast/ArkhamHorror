---
name: project_standalone_scenarios_earn_no_achievements
description: A standalone scenario game has gameMode = That scenario (no Campaign), so currentCampaign returns Nothing and no achievement can ever be earned — by design, as of 2026-09-11
metadata:
  type: project
---

`newScenario = newGame . That` (`Arkham/Game.hs`), so a standalone scenario game carries no
`Campaign` entity. `Arkham.Achievement.currentCampaign` maps `That _ -> Nothing`, which gates
`earnAchievement`, `achievementProgress` AND each campaign's `whenEligibleCampaign` — so the
whole detection module is off. **This is intended**: the user decided on 2026-09-11 to keep
achievements campaign-only rather than deriving a campaign id from the scenario code.

The reporter of #5691 hit exactly this — a standalone *Return to Extracurricular Activities*
named "Return to The Dunwich Legacy" (the New Game form defaults the name to the CAMPAIGN name
even in its standalone branch, so the name is not evidence of a campaign game). Tell from the
export: `jq '.campaignData.currentData.gameMode | keys'` is `["That"]`, and `grep -c '"This"'`
is 0. `These` encodes both keys.

The frontend used to offer the achievements toggle there anyway (`GameOptions.vue`'s
`effectiveCampaignId` ignored `fullCampaign === 'Standalone'`) and sent
`achievementsEnabled: true`; both now return null/false. The backend handler deliberately does
NOT re-check — `earnAchievement` and `CampaignLog.vue`'s tab already gate on the campaign, so
the flag is inert either way.
