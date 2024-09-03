module Arkham.Asset.Cards.OnyxPentacle4 (onyxPentacle4, OnyxPentacle4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Evade
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype OnyxPentacle4 = OnyxPentacle4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onyxPentacle4 :: AssetCard OnyxPentacle4
onyxPentacle4 = asset OnyxPentacle4 Cards.onyxPentacle4

instance HasModifiersFor OnyxPentacle4 where
  getModifiersFor (AbilityTarget _ ab) (OnyxPentacle4 attrs) | isSource attrs ab.source && ab.index == 1 = do
    modified
      attrs
      [ CanModify
        $ EnemyEvadeActionCriteria
        $ CriteriaOverride
        $ EnemyCriteria
        $ ThisEnemy
        $ EnemyCanBeEvadedBy (attrs.ability 1)
        <> EnemyAt (oneOf [YourLocation, ConnectedFrom YourLocation])
      | attrs.ready
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities OnyxPentacle4 where
  getAbilities (OnyxPentacle4 a) =
    [restrictedAbility a 1 ControlsThis evadeAction_]

instance RunMessage OnyxPentacle4 where
  runMessage msg a@(OnyxPentacle4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      isForced <- selectNone $ enemyAtLocationWith iid <> EnemyCanBeEvadedBy (attrs.ability 1)
      chooseOneM iid do
        when attrs.ready do
          forcedWhen isForced
            $ labeled
              "Exhaust Onyx Pentacle4 and place 1 doom on it to target any enemy at your location or a connecting location, and get +3 skill value for this evasion attempt."
            $ doStep 1 msg
        labeled "If you succeed by 2 or more, ready Onyx Pentacle and remove all doom from it."
          $ doStep 2 msg
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ Exhaust (toTarget attrs)
      placeDoom (attrs.ability 1) attrs 1
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid $ AnySkillValue 3
      evade <-
        mkChooseEvadeMatch sid iid (attrs.ability 1)
          $ evadeOverride
          $ EnemyCanBeEvadedBy (attrs.ability 1)
          <> oneOf
            [ EnemyAt (ConnectedFrom $ locationWithInvestigator iid)
            , enemyAtLocationWith iid
            ]
      chooseOneM iid do
        labeled "Use your {willpower}" $ push $ withSkillType #willpower evade
        labeled "get +1 {agility}" do
          skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 1)
          push evade
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      sid <- getRandom
      evade <- mkChooseEvade sid iid (attrs.ability 1)
      chooseOneM iid do
        labeled "Use your {willpower}" $ push $ withSkillType #willpower evade
        labeled "get +1 {agility}" do
          skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 1)
          push evade
      pure $ overAttrs (setMetaKey "option2" True) a
    PassedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      let option2 = getMetaKey "option2" attrs
      pushWhen attrs.exhausted $ Ready (toTarget attrs)
      pushWhen (attrs.doom > 0 && option2) $ RemoveAllDoom (attrs.ability 1) (toTarget attrs)
      pure a
    _ -> OnyxPentacle4 <$> liftRunMessage msg attrs
