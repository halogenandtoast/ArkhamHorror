module Arkham.Asset.Cards.GrimMemoir (grimMemoir, GrimMemoir (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Investigate
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype GrimMemoir = GrimMemoir AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grimMemoir :: AssetCard GrimMemoir
grimMemoir = asset GrimMemoir Cards.grimMemoir

instance HasAbilities GrimMemoir where
  getAbilities (GrimMemoir a) = [restrictedAbility a 1 ControlsThis $ investigateAction $ assetUseCost a Secret 1]

instance RunMessage GrimMemoir where
  runMessage msg a@(GrimMemoir attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (SkillModifier #intellect 2)
      pushM $ mkInvestigate iid (attrs.ability 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      chooseOneM iid do
        labeled "Draw 1 card" $ drawCardsIfCan iid (attrs.ability 1) 1
        labeled "Do not draw card" nothing
      pure a
    _ -> GrimMemoir <$> liftRunMessage msg attrs
