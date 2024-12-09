module Arkham.Asset.Assets.OnyxPentacle (onyxPentacle, OnyxPentacle (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Evade
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OnyxPentacle = OnyxPentacle AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onyxPentacle :: AssetCard OnyxPentacle
onyxPentacle = asset OnyxPentacle Cards.onyxPentacle

instance HasModifiersFor OnyxPentacle where
  getModifiersFor (OnyxPentacle a) = case a.controller of
    Just iid | a.ready -> do
      selectOne (AbilityIs (toSource a) 1) >>= \case
        Nothing -> pure mempty
        Just ab ->
          modified_
            a
            (AbilityTarget iid ab)
            [ CanModify
                $ EnemyEvadeActionCriteria
                $ CriteriaOverride
                $ EnemyCriteria
                $ ThisEnemy
                $ EnemyCanBeEvadedBy (a.ability 1)
                <> at_ (oneOf [YourLocation, ConnectedFrom YourLocation])
            ]
    _ -> pure mempty

instance HasAbilities OnyxPentacle where
  getAbilities (OnyxPentacle a) =
    [restrictedAbility a 1 ControlsThis evadeAction_]

instance RunMessage OnyxPentacle where
  runMessage msg a@(OnyxPentacle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      isForced <- selectNone $ enemyAtLocationWith iid <> EnemyCanBeEvadedBy (attrs.ability 1)
      chooseOneM iid do
        when attrs.ready do
          forcedWhen isForced
            $ labeled
              "Exhaust Onyx Pentacle and place 1 doom on it to target any enemy at your location or a connecting location, and get +1 skill value for this evasion attempt."
            $ doStep 1 msg
        labeled "If you succeed by 2 or more, remove 1 doom from Onyx Pentacle." $ doStep 2 msg
      pure $ overAttrs (unsetMetaKey "option2") a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      push $ Exhaust (toTarget attrs)
      placeDoom (attrs.ability 1) attrs 1
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid $ AnySkillValue 1
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
      when (attrs.doom > 0 && option2) $ removeDoom (attrs.ability 1) attrs 1
      pure a
    _ -> OnyxPentacle <$> liftRunMessage msg attrs
