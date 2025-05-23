module Arkham.Asset.Assets.LightningGun5 (lightningGun5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype LightningGun5 = LightningGun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightningGun5 :: AssetCard LightningGun5
lightningGun5 = asset LightningGun5 Cards.lightningGun5

instance HasAbilities LightningGun5 where
  getAbilities (LightningGun5 a) = [restricted a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage LightningGun5 where
  runMessage msg a@(LightningGun5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 2, SkillModifier #combat 5]
      chooseFightEnemy sid iid source
      pure a
    _ -> LightningGun5 <$> liftRunMessage msg attrs
