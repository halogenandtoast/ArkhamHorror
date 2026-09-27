---
title: project_campaign_attrs_replaced_investigators
description: Campaign attrs (decks/storyCards) keep entries for dead/replaced investigators; lists derived from their keys must be filtered to present investigators
---

Campaign `attrs.decks` and `attrs.storyCards` are keyed by InvestigatorId and **retain entries for investigators who were killed/driven insane and replaced** mid-campaign. Any helper that derives an investigator list from those keys (rather than from `getInvestigators`/`allInvestigators`) will return stale ids with no game entity — passing one to `getPlayer`/`getInvestigator` throws `MissingEntity "Unknown investigator"`.

TFA `getPoisonedInvestigators` read Poisoned (card `04102`) from `decks`+`storyCards` keys and returned replaced investigators; `storyOnlyBuild`'s `getPlayer` then crashed (issue #4956). Fix: made it monadic and `filter (\`elem\` present)` against `allInvestigators`. The supply helpers (`getInvestigatorsWithSupply`, etc.) were already safe because they iterate `getInvestigators` (= `select UneliminatedInvestigator`).

**Why:** the engine never prunes dead investigators' campaign-card maps. **How to apply:** when reading per-investigator data straight off campaign attrs, intersect with the live roster before resolving players/story text. Related: [[project_tfa_poison_storycards]].
