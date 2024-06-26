module Arkham.Asset.Cards.TrustyBullwhip (trustyBullwhip, TrustyBullwhip (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Modifier

newtype TrustyBullwhip = TrustyBullwhip AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trustyBullwhip :: AssetCard TrustyBullwhip
trustyBullwhip = asset TrustyBullwhip Cards.trustyBullwhip

instance HasAbilities TrustyBullwhip where
  getAbilities (TrustyBullwhip a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill [#fight] #agility $ ActionCost 1]

instance RunMessage TrustyBullwhip where
  runMessage msg a@(TrustyBullwhip attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- aspect iid source (#agility `InsteadOf` #combat) (mkChooseFight iid source)
      pushAll $ leftOr chooseFight
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      when attrs.ready do
        getSkillTestTarget >>= \case
          Just (EnemyTarget eid) -> do
            canEvade <- eid <=~> EnemyCanBeEvadedBy (attrs.ability 1)
            chooseOne iid
              $ [Label "Automatically evade the enemy" [Exhaust (toTarget attrs), EnemyEvaded iid eid] | canEvade]
              <> [ Label
                    "Deal +1 damage for this attack"
                    [Exhaust (toTarget attrs), Msg.skillTestModifier (attrs.ability 1) iid (DamageDealt 1)]
                 , Label "Do nothing" []
                 ]
          _ -> error "impossible"
      pure a
    _ -> TrustyBullwhip <$> lift (runMessage msg attrs)
