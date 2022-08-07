module Arkham.Skill.Cards.Perception where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Runner
import Arkham.Target

newtype Perception = Perception SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perception :: SkillCard Perception
perception = skill Perception Cards.perception

instance RunMessage Perception where
  runMessage msg s@(Perception attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ push (DrawCards skillOwner 1 False)
    _ -> Perception <$> runMessage msg attrs
