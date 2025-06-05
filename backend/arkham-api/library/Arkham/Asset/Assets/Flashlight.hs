module Arkham.Asset.Assets.Flashlight (flashlight) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.SkillTest.Lifted (investigate_)
import Arkham.Modifier

newtype Flashlight = Flashlight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashlight :: AssetCard Flashlight
flashlight = asset Flashlight Cards.flashlight

instance HasAbilities Flashlight where
  getAbilities (Flashlight x) = [investigateAbility x 1 (assetUseCost x #supply 1) ControlsThis]

instance RunMessage Flashlight where
  runMessage msg a@(Flashlight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      withLocationOf iid \lid -> skillTestModifier sid (attrs.ability 1) lid (ShroudModifier (-2))
      investigate_ sid iid (attrs.ability 1)
      pure a
    _ -> Flashlight <$> liftRunMessage msg attrs
