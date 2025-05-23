module Arkham.Skill.Cards.SurvivalInstinct (survivalInstinct) where

import Arkham.Action qualified as Action
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SurvivalInstinct = SurvivalInstinct SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct :: SkillCard SurvivalInstinct
survivalInstinct = skill SurvivalInstinct Cards.survivalInstinct

instance RunMessage SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs) = runQueueT $ case msg of
    PassedSkillTest iid (Just Action.Evade) _ (isTarget attrs -> True) _ _ -> do
      enemies <- select $ enemyEngagedWith iid
      canDisengage <- iid <=~> InvestigatorCanDisengage

      when (notNull enemies && canDisengage) do
        chooseOneM iid do
          labeled "Disengage from each other enemy" $ for_ enemies (disengageEnemy iid)
          labeled "Skip" nothing

      locations <- getConnectedMoveLocations iid (toSource attrs)
      chooseOrRunOneM iid do
        labeled "Do not move to a connecting location" nothing
        targets locations (moveTo attrs iid)
      pure s
    _ -> SurvivalInstinct <$> liftRunMessage msg attrs
