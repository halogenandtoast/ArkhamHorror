---
title: project_transformed_investigator_identity
description: Transformed investigators (Yithian/Homunculus/Shattered) keep their original investigatorId; InvestigatorIs matches via it
---

When an investigator transforms — `becomeYithian` (City of Archives, cardCode `04244`, `YithianForm`), `becomeHomunculus` (`11068b`), `becomeShatteredSelf` (`10661`) in `Arkham/Investigator.hs` — only `investigatorCardCode`/stats change; **`investigatorId` is preserved** and equals the original investigator's card code (`InvestigatorId` is a newtype over `CardCode`).

The `InvestigatorIs cardCode` matcher in `Arkham/Game.hs` therefore checks `toCardCode a == cardCode || (form is Yithian/Homunculus/Shattered && coerce a.id == cardCode) || (TransfiguredForm c && c == cardCode)`. Before #4822 it ignored the altered forms, so `investigatorIs lukeRobinson` failed for a "Body of a Yithian" Luke and trapped him in the Dream-Gate (forced-exit ability gated on that matcher).

**How to apply:** when a card targets a specific investigator via `investigatorIs X`, remember it now follows the investigator through transformed forms via the preserved id. Don't reintroduce per-card `cardCode == ...` checks that ignore form.
