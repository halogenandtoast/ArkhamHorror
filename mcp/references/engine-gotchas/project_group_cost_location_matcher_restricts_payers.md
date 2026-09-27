---
name: project_group_cost_location_matcher_restricts_payers
description: The LocationMatcher on GroupClueCost/GroupResourceCost/GroupDiscardCost picks who may PAY, not where the ability is usable — `be a` silently makes an "as a group" cost unaffordable when the clues sit elsewhere (#5639)
metadata:
  type: project
---

`GroupClueCost GameValue LocationMatcher` (and the `GroupResourceCost`/`GroupDiscardCost`
siblings) use the matcher as `select $ InvestigatorAt lm` to build the set of investigators who
may contribute — in the affordability check (`Arkham/Helpers/Cost.hs:501`) and again in the
payment step (`Arkham/ActiveCost.hs:1266`). It has nothing to do with who may *take* the ability;
that is the ability's own `Here`/`OnSameLocation` criterion.

Consequence: `GroupClueCost n (be a)` on a location whose printed text is a bare
"Spend X [per_investigator] clues, as a group" makes the ability **vanish** whenever the clues are
held by investigators standing somewhere else. There is no error and no log line — the cost is
simply unaffordable, so the ability is filtered out of the choice list.

**Why:** the Rules Reference on costs says "If the investigators are instructed to pay a cost as a
group, each investigator … may contribute" — no location restriction. FFG writes the restriction
explicitly when it exists ("**Investigators at this location** spend 1 [per_investigator] clues,
as a group").

**How to apply:** read the printed `real_text` before choosing the matcher.
- text says "**investigators at this location/at X** spend …" → `(be a)` / `locationIs …`
- text says only "spend …, as a group" → **`Anywhere`**

Found via #5639 (Core of the Vault *(Heart of the Machine)*, `11595`) and the same bug in
Glyph Orrery (`11662`); both fixed to `Anywhere`. Correct `(be a)` users to compare against:
Moving Platform `11594`, East/West Antechamber `11619`/`11620`, Tillinghast Esoterica `11509`.

Related: [[project_card_options_system]], [[project_playability_uses_active_investigator]].
