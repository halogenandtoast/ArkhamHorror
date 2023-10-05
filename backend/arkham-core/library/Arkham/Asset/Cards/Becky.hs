module Arkham.Asset.Cards.Becky (
  becky,
  Becky (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

-- N.B: The constant ability is handled on Tommy Muldoon

newtype Becky = Becky AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

becky :: AssetCard Becky
becky = asset Becky Cards.becky

instance HasAbilities Becky where
  getAbilities (Becky a) = [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage Becky where
  runMessage msg a@(Becky attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat 2]
        , chooseFightEnemy iid source #combat
        ]
      pure a
    _ -> Becky <$> runMessage msg attrs
