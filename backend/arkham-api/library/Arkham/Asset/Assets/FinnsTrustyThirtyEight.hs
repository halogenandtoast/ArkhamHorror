module Arkham.Asset.Assets.FinnsTrustyThirtyEight (finnsTrustyThirtyEight, FinnsTrustyThirtyEight (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype FinnsTrustyThirtyEight = FinnsTrustyThirtyEight AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finnsTrustyThirtyEight :: AssetCard FinnsTrustyThirtyEight
finnsTrustyThirtyEight = asset FinnsTrustyThirtyEight Cards.finnsTrustyThirtyEight

instance HasModifiersFor FinnsTrustyThirtyEight where
  getModifiersFor (FinnsTrustyThirtyEight a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      EnemyTarget eid <- MaybeT getSkillTestTarget
      guardM $ (== iid) <$> MaybeT getSkillTestInvestigator
      engagedEnemies <- lift $ select $ EnemyIsEngagedWith $ InvestigatorWithId iid
      guard $ eid `notElem` engagedEnemies
      pure [DamageDealt 1]

instance HasAbilities FinnsTrustyThirtyEight where
  getAbilities (FinnsTrustyThirtyEight a) = [fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis]

instance RunMessage FinnsTrustyThirtyEight where
  runMessage msg a@(FinnsTrustyThirtyEight attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifier sid source iid (SkillModifier #combat 2)
      pushAll [enabled, chooseFight]
      pure a
    _ -> FinnsTrustyThirtyEight <$> runMessage msg attrs
