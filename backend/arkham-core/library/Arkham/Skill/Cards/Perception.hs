module Arkham.Skill.Cards.Perception where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Perception = Perception SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

perception :: SkillCard Perception
perception = skill Perception Cards.perception

instance RunMessage Perception where
  runMessage msg s@(Perception attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      pushM $ drawCards (skillOwner attrs) attrs 1
      pure s
    _ -> Perception <$> runMessage msg attrs
