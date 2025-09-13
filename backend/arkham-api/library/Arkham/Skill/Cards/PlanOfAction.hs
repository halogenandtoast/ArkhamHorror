module Arkham.Skill.Cards.PlanOfAction (planOfAction) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

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
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      n <- length <$> selectAgg id InvestigatorActionsTaken TurnInvestigator
      when (n >= 0 && n < 2) do 
        skillTestResultOption "Plan of Action" $ drawCards iid attrs 1
      pure s
    _ -> PlanOfAction <$> liftRunMessage msg attrs
