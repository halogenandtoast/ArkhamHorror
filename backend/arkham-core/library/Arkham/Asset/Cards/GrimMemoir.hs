module Arkham.Asset.Cards.GrimMemoir (grimMemoir, GrimMemoir (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Message qualified as Msg
import Arkham.Investigate
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
      mmsg <- Msg.drawCardsIfCan iid (attrs.ability 1) 1
      for_ mmsg \draw -> chooseOne iid $ [Label "Draw 1 card" [draw], Label "Do not draw card" []]
      pure a
    _ -> GrimMemoir <$> liftRunMessage msg attrs
