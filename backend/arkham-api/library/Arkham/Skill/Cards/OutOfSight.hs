module Arkham.Skill.Cards.OutOfSight (outOfSight) where

import Arkham.ForMovement
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype OutOfSight = OutOfSight SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfSight :: SkillCard OutOfSight
outOfSight = skill OutOfSight Cards.outOfSight

instance RunMessage OutOfSight where
  runMessage msg s@(OutOfSight attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ n | n >= 2 -> do
      chooseOneM iid do
        labeled "Disengage from each enemy and move to a connecting location." $ doStep 1 msg
        withI18n skip_
      pure s
    DoStep 1 (PassedSkillTest iid _ _ (isTarget attrs -> True) _ _) -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      locations <-
        getCanMoveToMatchingLocations iid attrs
          $ ConnectedFrom ForMovement (locationWithInvestigator iid)
          <> #revealed
      chooseTargetM iid locations (moveTo (attrs.ability 1) iid)
      pure s
    _ -> OutOfSight <$> liftRunMessage msg attrs
