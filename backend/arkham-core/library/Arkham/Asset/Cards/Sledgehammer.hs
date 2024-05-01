module Arkham.Asset.Cards.Sledgehammer (sledgehammer, Sledgehammer (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Sledgehammer = Sledgehammer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sledgehammer :: AssetCard Sledgehammer
sledgehammer = asset Sledgehammer Cards.sledgehammer

instance HasAbilities Sledgehammer where
  getAbilities (Sledgehammer a) = [restrictedAbility a 1 ControlsThis fightAction_, restrictedAbility a 2 ControlsThis (ActionAbility [#fight] (ActionCost 2))]

instance RunMessage Sledgehammer where
  runMessage msg a@(Sledgehammer attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat (-1)], chooseFight]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt 2, SkillModifier #combat 2], chooseFight]
      pure a
    _ -> Sledgehammer <$> runMessage msg attrs