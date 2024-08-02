module Arkham.Asset.Cards.Katana (katana, Katana (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight (withSkillType)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Katana = Katana AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

katana :: AssetCard Katana
katana = asset Katana Cards.katana

instance HasAbilities Katana where
  getAbilities (Katana a) =
    [ restrictedAbility a 1 ControlsThis fightAction_
    , restrictedAbility a 2 ControlsThis $ FastAbility' (exhaust a) [#fight]
    ]

instance RunMessage Katana where
  runMessage msg a@(Katana attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n == 2 -> do
      when attrs.ready do
        withSkillTest \sid -> do
          chooseOneM iid do
            labeled "Exhaust Katana to deal +2 damage for this attack" do
              skillTestModifier sid (attrs.ability 1) iid (DamageDealt 2)
              push $ Exhaust (toTarget attrs)
            labeled "Do not exhaust" nothing
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      chooseFightEnemyEdit sid iid (attrs.ability 2) (withSkillType #agility)
      pure a
    _ -> Katana <$> liftRunMessage msg attrs
