module Arkham.Asset.Assets.StrangeSolutionFreezingVariant4 (
  strangeSolutionFreezingVariant4,
  StrangeSolutionFreezingVariant4 (..),
) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Evade
import Arkham.Prelude

newtype StrangeSolutionFreezingVariant4 = StrangeSolutionFreezingVariant4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionFreezingVariant4 :: AssetCard StrangeSolutionFreezingVariant4
strangeSolutionFreezingVariant4 =
  asset StrangeSolutionFreezingVariant4 Cards.strangeSolutionFreezingVariant4

instance HasAbilities StrangeSolutionFreezingVariant4 where
  getAbilities (StrangeSolutionFreezingVariant4 attrs) =
    [restrictedAbility attrs 1 ControlsThis $ evadeAction $ assetUseCost attrs Supply 1]

instance HasModifiersFor StrangeSolutionFreezingVariant4 where
  getModifiersFor (StrangeSolutionFreezingVariant4 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      source <- MaybeT getSkillTestSource
      guard $ isSource a source
      Action.Evade <- MaybeT getSkillTestAction
      pure [BaseSkillOf #agility 6]

instance RunMessage StrangeSolutionFreezingVariant4 where
  runMessage msg a@(StrangeSolutionFreezingVariant4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkChooseEvade sid iid (attrs.ability 1)
      pure a
    _ -> StrangeSolutionFreezingVariant4 <$> runMessage msg attrs
