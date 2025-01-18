module Arkham.Asset.Assets.MonstrousTransformation (monstrousTransformation) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType(..), controllerGets)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MonstrousTransformation = MonstrousTransformation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monstrousTransformation :: AssetCard MonstrousTransformation
monstrousTransformation = asset MonstrousTransformation Cards.monstrousTransformation

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
  getAbilities (MonstrousTransformation a) = [restricted a 1 ControlsThis $ fightAction (exhaust a)]

instance RunMessage MonstrousTransformation where
  runMessage msg a@(MonstrousTransformation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> MonstrousTransformation <$> liftRunMessage msg attrs
