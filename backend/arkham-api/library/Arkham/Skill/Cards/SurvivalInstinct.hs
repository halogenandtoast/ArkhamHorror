module Arkham.Skill.Cards.SurvivalInstinct (survivalInstinct) where

import Arkham.Action qualified as Action
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype SurvivalInstinct = SurvivalInstinct SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct :: SkillCard SurvivalInstinct
survivalInstinct = skill SurvivalInstinct Cards.survivalInstinct

instance RunMessage SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              when (st.action == Just Action.Evade) do
                enemies <- select $ enemyEngagedWith st.investigator
                canDisengage <- st.investigator <=~> InvestigatorCanDisengage

                when (notNull enemies && canDisengage) do
                  provideSkillTestResultOption attrs exclusions "Survival Instinct" do
                    chooseOneM st.investigator do
                      labeled "Disengage from each other enemy" $ for_ enemies (disengageEnemy st.investigator)
                      labeled "Skip" nothing

                locations <- getConnectedMoveLocations st.investigator (toSource attrs)
                chooseOrRunOneM st.investigator do
                  labeled "Do not move to a connecting location" nothing
                  targets locations (moveTo attrs st.investigator)
            _ -> pure ()
      pure s
    _ -> SurvivalInstinct <$> liftRunMessage msg attrs
