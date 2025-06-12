module Arkham.Skill.Cards.ExpeditiousRetreat1 (expeditiousRetreat1) where

import Arkham.Action qualified as Action
import Arkham.Constants
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype ExpeditiousRetreat1 = ExpeditiousRetreat1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditiousRetreat1 :: SkillCard ExpeditiousRetreat1
expeditiousRetreat1 = skill ExpeditiousRetreat1 Cards.expeditiousRetreat1

instance HasModifiersFor ExpeditiousRetreat1 where
  getModifiersFor (ExpeditiousRetreat1 a) = maybeModified_ a a.cardId do
    guardM isBasicEvade
    pure [AddSkillIcons [#agility, #agility]]

instance RunMessage ExpeditiousRetreat1 where
  runMessage msg s@(ExpeditiousRetreat1 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      void $ runMaybeT do
        Action.Evade <- MaybeT getSkillTestAction
        AbilitySource (EnemySource eid) AbilityEvade <- MaybeT getSkillTestAbilitySource
        iid <- MaybeT getSkillTestInvestigator
        enemies <-
          select $ enemyAtLocationWith iid <> not_ (EnemyWithId eid) <> CanEvadeEnemy (toSource iid)
        lift $ chooseTargetM iid enemies $ automaticallyEvadeEnemy iid
      pure s
    _ -> ExpeditiousRetreat1 <$> liftRunMessage msg attrs
