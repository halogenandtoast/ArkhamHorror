module Arkham.Asset.Assets.OnyxPentacle (onyxPentacle) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Evade
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OnyxPentacle = OnyxPentacle AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onyxPentacle :: AssetCard OnyxPentacle
onyxPentacle = asset OnyxPentacle Cards.onyxPentacle

instance HasModifiersFor OnyxPentacle where
  getModifiersFor (OnyxPentacle a) = for_ a.controller \iid ->
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
          <> at_ (oneOf [YourLocation, connectedFrom YourLocation])
      ]

instance HasAbilities OnyxPentacle where
  getAbilities (OnyxPentacle a) = [restricted a 1 ControlsThis evadeAction_]

instance RunMessage OnyxPentacle where
  runMessage msg a@(OnyxPentacle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      isForced <- selectNone $ enemyAtLocationWith iid <> EnemyCanBeEvadedBy (attrs.ability 1)
      chooseOneM iid do
        when attrs.ready do
          forcedWhen isForced
            $ (cardI18n $ labeled' "onyxPentacle.exhaustOnyxPentacleAndPlace1DoomOnItToTargetAnyEnemyAtYourLo")
            $ doStep 1 msg
        (cardI18n $ labeled' "onyxPentacle.ifYouSucceedBy2OrMoreRemove1DoomFromOnyxPentacle") $ doStep 2 msg
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      exhaustThis attrs
      placeDoom (attrs.ability 1) attrs 1
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid $ AnySkillValue 1
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
      when (attrs.doom > 0 && option2) $ removeDoom (attrs.ability 1) attrs 1
      pure a
    _ -> OnyxPentacle <$> liftRunMessage msg attrs
