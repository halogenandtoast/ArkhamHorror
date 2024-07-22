module Arkham.Asset.Cards.CeremonialSickle (ceremonialSickle, CeremonialSickle (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype CeremonialSickle = CeremonialSickle AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ceremonialSickle :: AssetCard CeremonialSickle
ceremonialSickle = asset CeremonialSickle Cards.ceremonialSickle

instance HasAbilities CeremonialSickle where
  getAbilities (CeremonialSickle a) =
    [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage CeremonialSickle where
  runMessage msg a@(CeremonialSickle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        when attrs.ready do
          labeled
            "Exhaust Ceremonial Sickle and place 1 doom on it to get +1 skill value and deal +1 damage for this attack."
            $ doStep 1 msg
        labeled "If this attack defeats an enemy, remove 1 doom from Ceremonial Sickle" $ doStep 2 msg
      fight <- mkChooseFight sid iid (attrs.ability 1)
      chooseOneM iid do
        labeled "Use your {willpower}" $ push $ withSkillType #willpower fight
        labeled "get +1 {combat}" do
          skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 1)
          push fight
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ Exhaust (toTarget attrs)
      placeDoom (attrs.ability 1) attrs 1
      withSkillTest \sid ->
        skillTestModifiers sid (attrs.ability 1) iid [AnySkillValue 1, DamageDealt 1]
      pure a
    DoStep 2 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      pure $ overAttrs (setMetaKey "option2" True) a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      let option2 = getMetaKey "option2" attrs
      when (attrs.doom > 0 && option2) $ removeDoom (attrs.ability 1) attrs 1
      pure a
    _ -> CeremonialSickle <$> liftRunMessage msg attrs
