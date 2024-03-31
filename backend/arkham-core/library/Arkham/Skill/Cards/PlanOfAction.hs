module Arkham.Skill.Cards.PlanOfAction (planOfAction, PlanOfAction (..)) where

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype PlanOfAction = PlanOfAction SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

planOfAction :: SkillCard PlanOfAction
planOfAction = skill PlanOfAction Cards.planOfAction

instance HasModifiersFor PlanOfAction where
  getModifiersFor target (PlanOfAction attrs) | attrs `is` target = do
    n <- length <$> selectAgg id InvestigatorActionsTaken Anyone
    pure
      $ toModifiers attrs
      $ [AddSkillIcons [#willpower, #agility] | n == 0]
      <> [AddSkillIcons [#combat, #intellect] | n >= 2]
  getModifiersFor _ _ = pure []

instance RunMessage PlanOfAction where
  runMessage msg s@(PlanOfAction attrs) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      mdrawing <- drawCardsIfCan iid attrs 1
      for_ mdrawing push
      pure s
    _ -> PlanOfAction <$> runMessage msg attrs
