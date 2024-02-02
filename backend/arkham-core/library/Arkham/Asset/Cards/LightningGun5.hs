module Arkham.Asset.Cards.LightningGun5 (lightningGun5, LightningGun5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype LightningGun5 = LightningGun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lightningGun5 :: AssetCard LightningGun5
lightningGun5 = asset LightningGun5 Cards.lightningGun5

instance HasAbilities LightningGun5 where
  getAbilities (LightningGun5 a) = [restrictedAbility a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage LightningGun5 where
  runMessage msg a@(LightningGun5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 2, SkillModifier #combat 5]
        , chooseFightEnemy iid (attrs.ability 1) #combat
        ]
      pure a
    _ -> LightningGun5 <$> runMessage msg attrs
