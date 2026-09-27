---
name: project_forcedchaostokenchange_token_target
description: ForcedChaosTokenChange only changes resolution when it lands on ChaosTokenTarget; the investigator-target copy is display-only
metadata:
  type: project
---

`ForcedChaosTokenChange old [new]` ("treat X as if it were Y") has **two** readers, and only one of them affects the game:

- `Arkham.Helpers.ChaosToken.getModifiedChaosTokenFace` reads modifiers off **`ChaosTokenTarget`**. This is the gameplay path — `SkillTest/Runner.hs` and `Scenario.hs` resolve revealed faces through it.
- `Game.hs`'s `withModifiers` for the wire reads the same modifier type off the **revealing investigator's** target and folds it into the `modifiedFaces` JSON field. That is display only.

So a modifier attached to an `InvestigatorTarget` makes the client *show* the substituted face while the engine still resolves the printed one. Always attach via a chaos-token query:

```haskell
modifySelect source (ChaosTokenRevealedBy $ be iid) [ForcedChaosTokenChange #eldersign [#autofail]]
```

`ChaosTokenRevealedBy` takes a full `InvestigatorMatcher`, so a campaign-log-driven substitution needs no per-investigator loop — one `modifySelect` with `investigatorWithRecord SomeKey` covers everyone.

For campaign-duration substitutions, note that `campaignModifiers :: Map InvestigatorId [Modifier]` is investigator-keyed and therefore hits the display-only path. Give the campaign its own `HasModifiersFor` instance instead (drop `HasModifiersFor` from the newtype deriving list, call `getModifiersFor a` for the attrs, then add the token query). The campaign entity IS consulted during `preloadModifiers`. The Drowned City's "Walk in Faith" failure (elder thing → auto-fail for the rest of the campaign) works this way, derived from the `LostTheirFaith` record so it survives every scenario.

Related: [[project_campaign_modifiers_for_all]], [[project_upgradedeck_replaces_campaign_deck]]
