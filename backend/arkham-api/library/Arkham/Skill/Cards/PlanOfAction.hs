module Arkham.Skill.Cards.PlanOfAction (planOfAction, PlanOfAction (..)) where

import Arkham.Helpers.Modifiers
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
    modifySelf attrs.cardId
      $ [AddSkillIcons [#willpower, #agility] | n == 0]
      <> [AddSkillIcons [#combat, #intellect] | n >= 2]

instance RunMessage PlanOfAction where
  runMessage msg s@(PlanOfAction attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      drawCardsIfCan iid attrs 1
      pure s
    _ -> PlanOfAction <$> liftRunMessage msg attrs
