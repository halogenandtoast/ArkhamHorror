module Arkham.Skill.Cards.Overpower where

import Arkham.Classes
import Arkham.Message
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Overpower = Overpower SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overpower :: SkillCard Overpower
overpower = skill Overpower Cards.overpower

instance RunMessage Overpower where
  runMessage msg s@(Overpower attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      push $ drawCards (skillOwner attrs) attrs 1
      pure s
    _ -> Overpower <$> runMessage msg attrs
