module Arkham.Asset.Cards.Sledgehammer4 (sledgehammer4, Sledgehammer4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Sledgehammer4 = Sledgehammer4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sledgehammer4 :: AssetCard Sledgehammer4
sledgehammer4 = asset Sledgehammer4 Cards.sledgehammer4

instance HasAbilities Sledgehammer4 where
  getAbilities (Sledgehammer4 a) =
    [ restrictedAbility a 1 ControlsThis fightAction_
    , restrictedAbility a 2 ControlsThis (ActionAbility [#fight] (ActionCost 3))
    ]

instance RunMessage Sledgehammer4 where
  runMessage msg a@(Sledgehammer4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 1], chooseFight]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifiers sid source iid [DamageDealt 5, SkillModifier #combat 5], chooseFight]
      pure a
    _ -> Sledgehammer4 <$> runMessage msg attrs
