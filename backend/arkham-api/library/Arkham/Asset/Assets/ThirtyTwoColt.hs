module Arkham.Asset.Assets.ThirtyTwoColt (thirtyTwoColt) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype ThirtyTwoColt = ThirtyTwoColt AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirtyTwoColt :: AssetCard ThirtyTwoColt
thirtyTwoColt = asset ThirtyTwoColt Cards.thirtyTwoColt

instance HasAbilities ThirtyTwoColt where
  getAbilities (ThirtyTwoColt a) = [fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis]

instance RunMessage ThirtyTwoColt where
  runMessage msg a@(ThirtyTwoColt attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> ThirtyTwoColt <$> liftRunMessage msg attrs
