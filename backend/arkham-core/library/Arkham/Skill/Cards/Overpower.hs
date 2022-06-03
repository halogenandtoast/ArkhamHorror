module Arkham.Skill.Cards.Overpower where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Attrs
import Arkham.Target

newtype Overpower = Overpower SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overpower :: SkillCard Overpower
overpower = skill Overpower Cards.overpower

instance RunMessage Overpower where
  runMessage msg s@(Overpower attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ push (DrawCards skillOwner 1 False)
    _ -> Overpower <$> runMessage msg attrs
