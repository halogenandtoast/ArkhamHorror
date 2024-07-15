module Arkham.Asset.Cards.ImprovisedShield (improvisedShield, ImprovisedShield (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Matcher

newtype ImprovisedShield = ImprovisedShield AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisedShield :: AssetCard ImprovisedShield
improvisedShield = assetWith ImprovisedShield Cards.improvisedShield (healthL ?~ 3)

instance HasAbilities ImprovisedShield where
  getAbilities (ImprovisedShield a) = [restrictedAbility a 1 ControlsThis $ forced $ AssetDefeated #when ByAny (be a)]

instance RunMessage ImprovisedShield where
  runMessage msg a@(ImprovisedShield attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleIntoDeck iid attrs
      pure a
    _ -> ImprovisedShield <$> liftRunMessage msg attrs
