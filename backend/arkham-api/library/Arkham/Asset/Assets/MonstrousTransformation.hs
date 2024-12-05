module Arkham.Asset.Assets.MonstrousTransformation (MonstrousTransformation (..), monstrousTransformation) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype MonstrousTransformation = MonstrousTransformation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monstrousTransformation :: AssetCard MonstrousTransformation
monstrousTransformation = assetWith MonstrousTransformation Cards.monstrousTransformation (isStoryL .~ True)

instance HasModifiersFor MonstrousTransformation where
  getModifiersFor (MonstrousTransformation a) =
    controllerGets
      a
      [ BaseSkillOf #willpower 2
      , BaseSkillOf #intellect 2
      , BaseSkillOf #combat 5
      , BaseSkillOf #agility 5
      ]

instance HasAbilities MonstrousTransformation where
  getAbilities (MonstrousTransformation a) = [restrictedAbility a 1 ControlsThis $ fightAction (exhaust a)]

instance RunMessage MonstrousTransformation where
  runMessage msg a@(MonstrousTransformation attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifier sid source iid (DamageDealt 1)
      pushAll [enabled, chooseFight]
      pure a
    _ -> MonstrousTransformation <$> runMessage msg attrs
