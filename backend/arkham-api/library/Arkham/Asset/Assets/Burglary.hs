module Arkham.Asset.Assets.Burglary (burglary) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted

newtype Burglary = Burglary AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burglary :: AssetCard Burglary
burglary = asset Burglary Cards.burglary

instance HasAbilities Burglary where
  getAbilities (Burglary a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage Burglary where
  runMessage msg a@(Burglary attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure a
    SuccessfulInvestigationWith iid (isTarget attrs -> True) -> do
      gainResources iid (attrs.ability 1) 3
      pure a
    _ -> Burglary <$> liftRunMessage msg attrs
