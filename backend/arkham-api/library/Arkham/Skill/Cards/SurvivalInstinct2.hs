module Arkham.Skill.Cards.SurvivalInstinct2 (survivalInstinct2) where

import Arkham.Action qualified as Action
import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Location
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype SurvivalInstinct2 = SurvivalInstinct2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct2 :: SkillCard SurvivalInstinct2
survivalInstinct2 = skill SurvivalInstinct2 Cards.survivalInstinct2

instance RunMessage SurvivalInstinct2 where
  runMessage msg s@(SurvivalInstinct2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              when (st.action == Just Action.Evade) do
                enemies <- select EnemyEngagedWithYou
                locations <- getAccessibleLocations st.investigator attrs

                unless (null enemies && null locations) do
                  provideSkillTestResultOption attrs exclusions "Survival Instinct (2)" do
                    unless (null enemies) do
                      chooseOneM st.investigator do
                        labeled "Evade each other enemy" do
                          for_ enemies (push . EnemyEvaded st.investigator)
                        labeled "Skip" nothing

                    unless (null locations) $ chooseOrRunOneM st.investigator do
                      labeled "Do not move to a connecting location" nothing
                      targets locations (moveTo attrs st.investigator)
            _ -> pure ()
      pure s
    _ -> SurvivalInstinct2 <$> liftRunMessage msg attrs
