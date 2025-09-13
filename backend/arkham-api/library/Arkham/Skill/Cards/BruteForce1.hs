module Arkham.Skill.Cards.BruteForce1 (bruteForce1) where

import Arkham.Action qualified as Action
import Arkham.Constants
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype BruteForce1 = BruteForce1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bruteForce1 :: SkillCard BruteForce1
bruteForce1 = skill BruteForce1 Cards.bruteForce1

instance HasModifiersFor BruteForce1 where
  getModifiersFor (BruteForce1 a) = maybeModified_ a a.cardId do
    Action.Fight <- MaybeT getSkillTestAction
    AbilitySource (EnemySource _) AbilityAttack <- MaybeT getSkillTestAbilitySource
    pure [AddSkillIcons [#combat, #combat]]

instance RunMessage BruteForce1 where
  runMessage msg s@(BruteForce1 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ n | n >= 2 -> do
      void $ runMaybeT do
        Action.Fight <- MaybeT getSkillTestAction
        AbilitySource (EnemySource _) AbilityAttack <- MaybeT getSkillTestAbilitySource
        iid <- MaybeT getSkillTestInvestigator
        lift $ withSkillTest \sid -> skillTestModifier sid (toSource attrs) iid (DamageDealt 2)
      pure s
    _ -> BruteForce1 <$> liftRunMessage msg attrs
