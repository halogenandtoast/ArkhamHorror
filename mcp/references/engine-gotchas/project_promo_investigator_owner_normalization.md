---
title: project_promo_investigator_owner_normalization
description: "Promo/alt-art investigators (98xxx) loaded with raw id as entity but deck-load used to normalize card owner to base code, causing You/Self != pcOwner owner-check bugs (issue"
---

Promo/alt-art investigators (e.g. 98013 = Silas Marsh, mapped in `promoInvestigators` in `Arkham/Investigator.hs`) are loaded with their **raw** promo id as the investigator entity id (`Decklist.hs` stores `decklistInvestigatorId` un-normalized, so `You`/`Self` = 98013).

The bug (issue #5027): `Decklist.loadDecklistCards`/`loadExtraDeck` used to set card `pcOwner` via `setPlayerCardOwner (normalizeInvestigatorId ...)` → base code 07005, while the entity id stayed 98013. So any `SkillOwnedBy You` / owner-based check (Silas Marsh's return-committed-skill ability) missed the deck cards (owner 07005 ≠ id 98013). In-play-created cards (`setOwner iid`, `genCard`) used the raw id 98013, so ownership was inconsistent card-by-card. Also latent crash: `getInvestigator 07005` throws MissingEntity (promo fallback only maps promo→base, not reverse).

**Fix:** dropped `normalizeInvestigatorId` at deck load — card owner now = the exact loaded investigator id (promo or base), matching the entity id. Removed the now-unused `import Arkham.Investigator` from `Decklist.hs`. `normalizeInvestigatorId` is now call-site-free but still exported.

Caveat: only affects NEW deck loads. Already-serialized games keep the old mismatched (07005) ownership until reloaded — the reported game won't self-heal.

Mechanism detail: promo/alt-art investigators are registered via `withAlternate "98013"` on the base CardDef (`Investigator/Cards.hs`). `toCardCodePairs` expands to first-class entries for BOTH codes; the 98013 entry has `cdCardCode=98013`, `cdAlternateCardCodes=[07005]`, so the loaded entity id/cardCode/art = 98013 but `(toCardDef e).cardCodes = [98013,07005]`. PARALLEL investigators (90xxx, e.g. Rex 90078 / Agnes 90017) are SEPARATE defs with distinct rules — referenced by their own code and explicitly OR'd (`investigatorIs jim <> investigatorIs jimParallel`); do NOT fold them into alternates.

Second fix (broader audit): `InvestigatorIs cardCode` in `Game.hs` matched `toCardCode a == cardCode`, so `investigatorIs Cards.carolynFern` (= base 05001) missed promo Carolyn (98010) — her signature *To Fight The Black Wind* etc. Changed to `cardCode elem (toCardDef (toAttrs a)).cardCodes`. Single chokepoint — covers every `investigatorIs`/`InvestigatorIs` caller. Yithian/Homunculus/Shattered/Transfigured form cases retained as fallback.

Audited safe: deck upgrade paths (`ReplaceInvestigator`/`UpgradeDecklist`) preserve entity id via getInvestigator and load cards through `loadDecklistCards` (now consistent owner); no card does direct `toCardCode inv == "<base>"`; no `InvestigatorWithId "<literal>"`; investigator self-abilities key on entity (`isTarget attrs`) so promo abilities work.
