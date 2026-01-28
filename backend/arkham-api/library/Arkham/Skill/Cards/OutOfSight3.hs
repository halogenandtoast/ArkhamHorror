module Arkham.Skill.Cards.OutOfSight3 (outOfSight3) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype OutOfSight3 = OutOfSight3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfSight3 :: SkillCard OutOfSight3
outOfSight3 = skill OutOfSight3 Cards.outOfSight3

instance RunMessage OutOfSight3 where
  runMessage msg s@(OutOfSight3 attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 1 -> do
      chooseOneM iid do
        labeled "Disengage from each enemy and move to a revealed location up to 2 connections away."
          $ doStep 1 msg
        withI18n skip_
      pure s
    DoStep 1 (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      locations <-
        select $ LocationWithAccessiblePath (toSource attrs) 2 (InvestigatorWithId iid) #revealed
      chooseTargetM iid locations (moveTo (attrs.ability 1) iid)
      pure s
    _ -> OutOfSight3 <$> liftRunMessage msg attrs
