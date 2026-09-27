---
title: project_scarletkey_source_ownership
description: SourceOwnedBy/SourceUsedBy now resolves a ScarletKeySource to its bearer/controller investigator
---

`checkSourceOwner` in `Arkham/Helpers/Source.hs` (backing `SourceOwnedBy`/`SourceUsedBy`) had no case for `ScarletKeySource`, so a Scarlet Key's effect source resolved to nobody. That broke "card effect you control" triggers — e.g. Carolyn Fern's resource gain when she heals another investigator with The Last Blossom (issue #4948).

Fix: added a `ScarletKeySource sid` case resolving via the matcher layer — `select $ ScarletKeyOneOf [ScarletKeyWithInvestigator whoMatcher, ScarletKeyWithBearer whoMatcher]` then `sid \`elem\` owned`. Used the matcher (not the `ScarletKeyBearer`/`ScarletKeyPlacement` Field projection) to avoid an import cycle, since `Arkham.Campaigns.TheScarletKeys.Key.Types` is too heavy to import into `Helpers.Source`; `Key.Matcher` is cycle-safe.

Caveat: `ScarletKeyWithInvestigator` covers AttachedToInvestigator + AsIfUnderControlOf only — a key attached to a story asset doesn't resolve to that asset's controller. Not needed for #4948.
