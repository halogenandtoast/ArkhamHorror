---
title: project_circle_undone_achievements
description: Circle Undone (54) achievement detection; Black Throne R5/R6 record ordering + R6 signing-bug fix
---

Return to The Circle Undone achievements (campaign "54") live in
`Arkham.Campaign.Campaigns.TheCircleUndone.Achievements` (hooked at top of base
`TheCircleUndone` runMessage; Return-to delegates via `liftRunMessage`).

Before the Black Throne's four surviving wins all `record AzathothSlumbersForNow`
(that shared key = "won the campaign", used for Circle Expertise). To tell the
four apart:
- R2 Music (Pipers): unique `TheLeadInvestigatorHasJoinedThePipersOfAzathoth`.
- R5 Fine Print: `TheInvestigatorsSignedTheBlackBookOfAzathoth`.
- R6 Speak the Words Aloud ("duolA sdroW eht kaepS", reverse the incantation):
  reachable ONLY via `WhatMustBeDoneV2` (Return-to act, needs StrangeIncantation
  + BloodyTreeCarvings mementos).
- R3 Weaver: records ONLY slumbers → detected as "slumbers with none of the
  other three unique keys recorded".

Two real fixes were required in `BeforeTheBlackThrone.hs`:
1. R6 wrongly did `record TheInvestigatorsSignedTheBlackBookOfAzathoth` (copy-paste
   from R5; card text records only slumbers) — it mis-fired Fine Print. Replaced
   with a NEW key `TheInvestigatorsReversedTheIncantation` (added to
   `Campaigns/TheCircleUndone/Key.hs` + i18n in `en/theCircleUndone/base.json`
   `.key.theInvestigatorsReversedTheIncantation`).
2. The unique key must be recorded BEFORE `record AzathothSlumbersForNow` in each
   resolution, so the Weaver "no-other-ending-yet" check at slumbers-dispatch is
   correct (R5 was reordered; R2 already was; R6 written that way).

Faction-win achievements are the Union & Disillusion gameOver records:
New World Order = `TheTrueWorkOfTheSilverTwilightLodgeHasBegun`,
Immortality Sounds Nice = `TheCovenOfKeziahHoldsTheWorldInItsGrasp`.
Those two records are written ONLY at the loyal-faction-win resolutions (R2/R9),
so they also count as a campaign win for Circle Expertise (a loyal Lodge/Coven
win on Expert earns it) — not just the Black Throne `AzathothSlumbersForNow`
survivals. `checkCircleExpertise` is called from all three winning records.

MemberThese (10 mementos) and CaseClosed (4 *Fate stories) use the checklist
mechanism (`achievementChecklist` in Achievement.Types + `achievementProgress`),
not one-shot earns. Incursion detection = the `Incursion LocationId` Message.
