module Arkham.Skill.Cards.FightingLessons where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher.Types
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype FightingLessons = FightingLessons SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightingLessons :: SkillCard FightingLessons
fightingLessons = skill FightingLessons Cards.fightingLessons

instance HasModifiersFor FightingLessons where
  getModifiersFor (CardIdTarget cid) (FightingLessons attrs) | toCardId attrs == cid = do
    pure $ toModifiers attrs [CanCommitToSkillTestPerformedByAnInvestigatorAt Anywhere]
  getModifiersFor _ _ = pure []

instance RunMessage FightingLessons where
  runMessage msg (FightingLessons attrs) = FightingLessons <$> runMessage msg attrs
