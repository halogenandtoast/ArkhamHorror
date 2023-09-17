module Arkham.Asset.Cards.ThirtyTwoColt (
  thirtyTwoColt,
  ThirtyTwoColt (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype ThirtyTwoColt = ThirtyTwoColt AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thirtyTwoColt :: AssetCard ThirtyTwoColt
thirtyTwoColt = asset ThirtyTwoColt Cards.thirtyTwoColt

instance HasAbilities ThirtyTwoColt where
  getAbilities (ThirtyTwoColt a) =
    [fightAbility a 1 (ActionCost 1 <> assetUseCost a Ammo 1) ControlsThis]

instance RunMessage ThirtyTwoColt where
  runMessage msg a@(ThirtyTwoColt attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier (toAbilitySource attrs 1) iid (DamageDealt 1)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    _ -> ThirtyTwoColt <$> runMessage msg attrs
