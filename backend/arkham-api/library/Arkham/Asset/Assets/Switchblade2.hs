module Arkham.Asset.Assets.Switchblade2 (switchblade2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype Switchblade2 = Switchblade2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade2 :: AssetCard Switchblade2
switchblade2 = asset Switchblade2 Cards.switchblade2

instance HasAbilities Switchblade2 where
  getAbilities (Switchblade2 a) = [restricted a 1 ControlsThis fightAction_]

instance RunMessage Switchblade2 where
  runMessage msg a@(Switchblade2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid source
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (DamageDealt 1)
      pure a
    _ -> Switchblade2 <$> liftRunMessage msg attrs
