module Arkham.Asset.Assets.CeremonialSickle4 (ceremonialSickle4, CeremonialSickle4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype CeremonialSickle4 = CeremonialSickle4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ceremonialSickle4 :: AssetCard CeremonialSickle4
ceremonialSickle4 = asset CeremonialSickle4 Cards.ceremonialSickle4

instance HasAbilities CeremonialSickle4 where
  getAbilities (CeremonialSickle4 a) =
    [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage CeremonialSickle4 where
  runMessage msg a@(CeremonialSickle4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        when attrs.ready do
          (cardI18n $ labeled' "ceremonialSickle4.exhaustCeremonialSickleAndPlace1DoomOnItToGet3SkillValueAndD")
            $ doStep 1 msg
        (cardI18n $ labeled' "ceremonialSickle4.ifThisAttackDefeatsAnEnemyReadyCeremonialSickleAndRemoveAllD")
          $ doStep 2 msg
      fight <- mkChooseFight sid iid (attrs.ability 1)
      chooseOneM iid do
        (withI18n $ skillVar #willpower $ labeled' "useSkill") $ push $ withSkillType #willpower fight
        (withI18n $ countVar 1 $ skillVar #combat $ labeled' "getPlus") do
          skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 1)
          push fight
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      exhaustThis attrs
      placeDoom (attrs.ability 1) attrs 1
      thisSkillTestModifiers iid (attrs.ability 1) iid [AnySkillValue 3, DamageDealt 1]
      pure a
    DoStep 2 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      pure $ overAttrs (setMetaKey "option2" True) a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      when (getMetaKey "option2" attrs) do
        when attrs.exhausted $ ready attrs
        pushWhen (attrs.doom > 0) $ RemoveAllDoom (attrs.ability 1) (toTarget attrs)
      pure a
    _ -> CeremonialSickle4 <$> liftRunMessage msg attrs
