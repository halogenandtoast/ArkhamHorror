---
name: project_campaign_nextstep_must_read_normalized_step
description: "A campaign's nextStep must read (toAttrs a).normalizedStep — picking a lead investigator makes the step ScenarioStepWithOptions, which bare ScenarioStep patterns miss, so defaultNextStep returns Nothing and the campaign pushes GameOver (#5811)"
metadata:
  node_type: memory
  type: project
  originSessionId: 8eb2523e-ca15-472c-a259-b5166a3f7e9d
  modified: 2026-10-06T02:20:06.066Z
---

`CampaignSteps.hs` pattern synonyms are all bare `ScenarioStep "13001"`, but the
campaign's live step becomes `ScenarioStepWithOptions sid opts` as soon as a player
picks a lead investigator on the continue screen — `ContinueCampaign.vue:327` sends
`CampaignStepAnswer (extendWithOptions step {leadInvestigator})`. The scenario-side
runner rewrites it (`Scenario/Runner.hs:2196`) and `Campaign/Runner.hs` handles both
shapes (:99 and :110), but **`IsCampaign.nextStep` does not** — `defaultNextStep`
(`CampaignStep.hs:114`) has no `ScenarioStepWithOptions` case, falls to `Nothing`,
and `NextCampaignStep` (`Campaign/Runner.hs:547`) then pushes `GameOver`.

So reading the raw step silently ends the campaign after *every* scenario. Children
of Blood shipped this way (#5811): `nextStep a = case campaignStep (toAttrs a) of`.

**Why:** the convention `(toAttrs a).normalizedStep` (`Campaign/Types.hs:151` =
`(.normalize) . campaignStep`) landed in `62256f2c94` "Normalize next steps" (Oct
2025) and retrofitted the then-existing campaigns. Anything written later — Children
of Blood (Aug 2026) — had to pick it up by imitation, and nothing enforces it.

**How to apply:** a new campaign's `nextStep` reads `(toAttrs a).normalizedStep`,
never `campaignStep (toAttrs a)`. Same trap for any `CampaignStep (ScenarioStep _)`
case in a campaign's `runMessage` or achievements hook — use a
`normalizedCampaignStep -> ScenarioStep _` view pattern. Children of Blood's
achievements module had this too: `civilianDefeatedKey` never reset between
scenarios, leaking a River of Blood civilian defeat into New Horizons and Blood
Money. `StandaloneCampaign` reads the raw step legitimately — it only matches steps
normalization leaves alone.

Verified via `arkham-replay` on the #5811 export: `gameGameState` went `IsOver` →
`IsActive`, step `ScenarioStepWithOptions "13001"` →
`ContinueCampaignStep {nextStep = ScenarioStep "13031"}`.

Note `stack exec arkham-replay` can run a **stale** copy from
`backend/.stack-work/install/.../bin/`; `make api.watch` only relinks
`arkham-api/.stack-work/dist/aarch64-osx/ghc-9.14.1/build/arkham-replay/arkham-replay`.
Invoke that path directly when verifying a fix, or a no-op diff will look like the
fix failed. See [[feedback_verify_edited_module_in_build_log]].

Related: [[project_dual_continuecampaignstep_precedence]],
[[project_campaign_option_needs_its_own_handleoption_case]],
[[project_secrets_of_the_order_status]].
