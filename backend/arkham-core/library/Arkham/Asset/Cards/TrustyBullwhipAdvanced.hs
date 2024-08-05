module Arkham.Asset.Cards.TrustyBullwhipAdvanced (trustyBullwhipAdvanced, TrustyBullwhipAdvanced (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier

newtype TrustyBullwhipAdvanced = TrustyBullwhipAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trustyBullwhipAdvanced :: AssetCard TrustyBullwhipAdvanced
trustyBullwhipAdvanced = asset TrustyBullwhipAdvanced Cards.trustyBullwhipAdvanced

instance HasAbilities TrustyBullwhipAdvanced where
  getAbilities (TrustyBullwhipAdvanced a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill [#fight] #agility $ ActionCost 1]

instance RunMessage TrustyBullwhipAdvanced where
  runMessage msg a@(TrustyBullwhipAdvanced attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      chooseFight <- aspect iid source (#agility `InsteadOf` #combat) (mkChooseFight sid iid source)
      pushAll $ leftOr chooseFight
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTest \sid -> do
        when attrs.ready do
          getSkillTestTarget >>= \case
            Just (EnemyTarget eid) -> do
              canEvade <- eid <=~> EnemyCanBeEvadedBy (attrs.ability 1)
              chooseOne iid
                $ [ Label "Deal + 1 damage for this attack and Automatically evade the enemy" $ Exhaust (toTarget attrs)
                      : Msg.skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
                      : [EnemyEvaded iid eid | canEvade]
                  , Label "Do nothing" []
                  ]
            _ -> error "impossible"
      pure a
    _ -> TrustyBullwhipAdvanced <$> liftRunMessage msg attrs
