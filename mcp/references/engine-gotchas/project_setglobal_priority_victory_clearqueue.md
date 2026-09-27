---
title: project_setglobal_priority_victory_clearqueue
description: "Achievement store writes (SetGlobal) that coincide with a victory-enemy defeat get clearQueue'd; push them via Priority"
---

Campaign-store counter writes (`push $ SetGlobal CampaignTarget ...`) that fire during a message whose cascade `clearQueue`s get silently dropped — the plainly-pushed SetGlobal is wiped before it's popped, so `stored` reads never accumulate.

Concretely: a `Defeated` of a **victory enemy** (has `cdVictoryPoints`) runs an AddToVictory / AddedToVictory-window cascade that clearQueues; a per-defeat counter keyed on that Defeated never increments (every read returns 0). Symptom in specs: a debug trace shows the detection branch running N times but the stored value stuck at 0, and no `SetGlobal` ever appears in the processed-message log.

**Fix:** push store writes with Priority, same rationale as `earnAchievement`: `push $ Priority $ SetGlobal CampaignTarget k v`. Priority is popped before the rest of the cascade, so the write applies before any clear. NOTZ/Dunwich reference `setStore` uses plain push and works only because their counter enemies (Whippoorwill, Ghoul) have no victory cascade. Carcosa's `Fair Warning` (Royal Emissary ×3) forced the Priority version — see `Arkham.Campaign.Campaigns.ThePathToCarcosa.Achievements`.

Related: [[project_skip_all_triggers]], and the general "earn via earnAchievement's Priority push so clearQueue can't eat it" pattern in the implement-achievements skill.
