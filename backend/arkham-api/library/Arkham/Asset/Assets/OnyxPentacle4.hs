module Arkham.Asset.Assets.OnyxPentacle4 (onyxPentacle4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Evade
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OnyxPentacle4 = OnyxPentacle4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onyxPentacle4 :: AssetCard OnyxPentacle4
onyxPentacle4 = asset OnyxPentacle4 Cards.onyxPentacle4

instance HasModifiersFor OnyxPentacle4 where
  getModifiersFor (OnyxPentacle4 a) = for_ a.controller \iid ->
    modifiedWhen_
      a
      a.ready
      (AbilityTarget iid $ AbilityRef (toSource a) 1)
      [ CanModify
          $ EnemyEvadeActionCriteria
          $ CriteriaOverride
          $ EnemyCriteria
          $ ThisEnemy
          $ EnemyCanBeEvadedBy (a.ability 1)
          <> EnemyAt (oneOf [YourLocation, connectedFrom YourLocation])
      ]

instance HasAbilities OnyxPentacle4 where
  getAbilities (OnyxPentacle4 a) = [restricted a 1 ControlsThis evadeAction_]

instance RunMessage OnyxPentacle4 where
  runMessage msg a@(OnyxPentacle4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      isForced <- selectNone $ enemyAtLocationWith iid <> EnemyCanBeEvadedBy (attrs.ability 1)
      chooseOneM iid do
        when attrs.ready do
          forcedWhen isForced
            $ (cardI18n $ labeled' "onyxPentacle4.exhaustToTarget")
            $ doStep 1 msg
        (cardI18n $ labeled' "onyxPentacle4.succeedBy2")
          $ doStep 2 msg
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      exhaustThis attrs
      placeDoom (attrs.ability 1) attrs 1
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid $ AnySkillValue 3
      evade <-
        mkChooseEvadeMatch sid iid (attrs.ability 1)
          $ evadeOverride
          $ EnemyCanBeEvadedBy (attrs.ability 1)
          <> oneOf
            [ EnemyAt (connectedFrom $ locationWithInvestigator iid)
            , enemyAtLocationWith iid
            ]
      chooseOneM iid do
        (withI18n $ skillVar #willpower $ labeled' "useSkill") $ push $ withSkillType #willpower evade
        (withI18n $ countVar 1 $ skillVar #agility $ labeled' "getPlus") do
          skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 1)
          push evade
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      sid <- getRandom
      evade <- mkChooseEvade sid iid (attrs.ability 1)
      chooseOneM iid do
        (withI18n $ skillVar #willpower $ labeled' "useSkill") $ push $ withSkillType #willpower evade
        (withI18n $ countVar 1 $ skillVar #agility $ labeled' "getPlus") do
          skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 1)
          push evade
      pure $ overAttrs (setMetaKey "option2" True) a
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      let option2 = getMetaKey "option2" attrs
      pushWhen attrs.exhausted $ Ready (toTarget attrs)
      pushWhen (attrs.doom > 0 && option2) $ RemoveAllDoom (attrs.ability 1) (toTarget attrs)
      pure a
    _ -> OnyxPentacle4 <$> liftRunMessage msg attrs
