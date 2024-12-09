module Arkham.Skill.Cards.ExpeditiousRetreat1 (
  expeditiousRetreat1,
  ExpeditiousRetreat1 (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Classes
import Arkham.Constants
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype ExpeditiousRetreat1 = ExpeditiousRetreat1 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditiousRetreat1 :: SkillCard ExpeditiousRetreat1
expeditiousRetreat1 = skill ExpeditiousRetreat1 Cards.expeditiousRetreat1

instance HasModifiersFor ExpeditiousRetreat1 where
  getModifiersFor (ExpeditiousRetreat1 a) = maybeModified_ a (CardIdTarget $ toCardId a) do
    Action.Evade <- MaybeT getSkillTestAction
    AbilitySource (EnemySource _) AbilityAttack <- MaybeT getSkillTestSource
    pure [AddSkillIcons [#agility, #agility]]

instance RunMessage ExpeditiousRetreat1 where
  runMessage msg s@(ExpeditiousRetreat1 attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      mAction <- getSkillTestAction
      mSource <- getSkillTestSource
      mInvestigator <- getSkillTestInvestigator
      case (mAction, mSource, mInvestigator) of
        (Just Action.Evade, Just (AbilitySource (EnemySource eid) AbilityAttack), Just iid) -> do
          enemies <-
            select $ enemyAtLocationWith iid <> NotEnemy (EnemyWithId eid) <> CanEvadeEnemy (toSource iid)
          player <- getPlayer iid
          pushIfAny enemies $ chooseOne player $ targetLabels enemies (only . EnemyEvaded iid)
        _ -> pure ()
      pure s
    _ -> ExpeditiousRetreat1 <$> runMessage msg attrs
