module Arkham.Skill.Cards.PlanOfAction (planOfAction) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype PlanOfAction = PlanOfAction SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

planOfAction :: SkillCard PlanOfAction
planOfAction = skill PlanOfAction Cards.planOfAction

instance HasModifiersFor PlanOfAction where
  getModifiersFor (PlanOfAction attrs) = do
    n <- length <$> selectAgg id InvestigatorActionsTaken TurnInvestigator
    addSkillIcons attrs
      $ (guard (n == 0) *> [#willpower, #agility])
      <> (guard (n >= 2) *> [#combat, #intellect])

instance RunMessage PlanOfAction where
  runMessage msg s@(PlanOfAction attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              n <- length <$> selectAgg id InvestigatorActionsTaken TurnInvestigator
              when (n >= 0 && n < 2) do
                provideSkillTestResultOption attrs exclusions "Plan of Action" $ drawCards st.investigator attrs 1
            _ -> pure ()
      pure s
    _ -> PlanOfAction <$> liftRunMessage msg attrs
