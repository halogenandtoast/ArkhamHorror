---
title: "The lead investigator" after everyone is eliminated is the most recently appointed lead
date_added: 2026-08-17
source: FFG ruling (quoted in issue #5420)
affects:
  - lead investigator
  - The Man in the Pallid Mask
  - Curtain Call
  - scenario resolutions
---

# "The lead investigator" after everyone is eliminated is the most recently appointed lead

> If the lead investigator is eliminated, the remaining players (if any) choose a new lead
> investigator.

FFG follow-up ruling:

> For resolutions instructing "the lead investigator" to do something, the most recently appointed
> lead investigator would resolve that instruction. For your example, Pete would add the Man in the
> Pallid Mask to his deck, since he is the most recent lead investigator.

So when a resolution addresses "the lead investigator" but the whole party has been eliminated
(resigned, defeated, killed, driven insane), the instruction is resolved by whoever held the title
last — *not* by an arbitrary investigator and *not* by the original starting lead.

The engine already tracks this correctly: `ChooseLeadInvestigator` is pushed whenever the current
lead is eliminated, and it no-ops once nobody is left, so `gameLeadInvestigatorId` always holds the
most recently appointed lead.

## Affected cards / systems

- `getLead` / `getLeadMay` / `getLeadPlayer` — `backend/arkham-api/library/Arkham/Helpers/Query.hs`
- `LeadInvestigator` matcher and the elimination filter in `getInvestigatorsMatching` —
  `backend/arkham-api/library/Arkham/Game.hs`
- `ChooseLeadInvestigator` re-choose sites — `backend/arkham-api/library/Arkham/Investigator/Runner.hs`,
  `backend/arkham-api/library/Arkham/Investigator/Runner/Damage.hs`
- Every resolution that says "the lead investigator", e.g. The Man in the Pallid Mask (03059) in
  `Scenario/Scenarios/CurtainCall.hs`, `TheLastKing.hs`, `APhantomOfTruth.hs`, `ThePallidMask.hs`,
  `BlackStarsRise.hs`, `DimCarcosa.hs`

## Implementation status

- **`getLead` fallback**: ✏️ fixed (issue #5420). `LeadInvestigator` is filtered against the
  *uneliminated* roster inside `getInvestigatorsMatching`, so once the lead was eliminated the
  matcher went blank and `getLead` fell through to `selectOne (IncludeEliminated Anyone)` — which
  reads the entity `Map` in `InvestigatorId` order and therefore returned the lowest-id
  investigator. In issue #5420 Daisy (`c01002`) was the recorded lead but Roland (`c01001`) received
  The Man in the Pallid Mask. `getLeadMay` now consults `getRecordedLead`
  (`selectOne (IncludeEliminated LeadInvestigator)`) before the arbitrary fallback, which is kept
  only for the case where the recorded lead is no longer an entity at all.
- **`ChooseLeadInvestigator` re-choose checks**: ✏️ hardened. The three elimination sites asked
  `(== iid) <$> getLead`; they now ask `(== Just iid) <$> getRecordedLead` so "am I the lead?"
  reads `gameLeadInvestigatorId` directly instead of depending on the fallback chain.
- **Test**: added `Arkham.Helpers.QuerySpec` — Roland resigns first, then the lead resigns last;
  `getLead` must return the lead rather than the lower-id Roland.
  (`backend/arkham-api/tests/Arkham/Helpers/QuerySpec.hs`)

## Open questions / out of scope

- The fix is not retroactive: a campaign that already misassigned a story card needs a manual
  `RemoveCardFromDeckForCampaign` + `AddCampaignCardToDeck` repair.
- `Continuation.lead` on `ContinueCampaignStep` is a *different* lead — the one players pick on the
  ContinueCampaign screen for the next scenario. It being `Nothing` mid-campaign is normal.
