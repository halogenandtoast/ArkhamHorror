module Arkham.Asset.Assets.Switchblade (switchblade) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype Switchblade = Switchblade AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade :: AssetCard Switchblade
switchblade = asset Switchblade Cards.switchblade

instance HasAbilities Switchblade where
  getAbilities (Switchblade a) = [restricted a 1 ControlsThis fightAction_]

instance RunMessage Switchblade where
  runMessage msg a@(Switchblade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (DamageDealt 1)
      pure a
    _ -> Switchblade <$> liftRunMessage msg attrs
