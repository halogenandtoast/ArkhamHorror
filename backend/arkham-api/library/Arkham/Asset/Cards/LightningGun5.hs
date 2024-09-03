module Arkham.Asset.Cards.LightningGun5 (lightningGun5, LightningGun5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype LightningGun5 = LightningGun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightningGun5 :: AssetCard LightningGun5
lightningGun5 = asset LightningGun5 Cards.lightningGun5

instance HasAbilities LightningGun5 where
  getAbilities (LightningGun5 a) = [restrictedAbility a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage LightningGun5 where
  runMessage msg a@(LightningGun5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifiers sid source iid [DamageDealt 2, SkillModifier #combat 5], chooseFight]
      pure a
    _ -> LightningGun5 <$> runMessage msg attrs
