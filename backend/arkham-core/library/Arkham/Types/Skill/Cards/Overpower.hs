module Arkham.Types.Skill.Cards.Overpower where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Target

newtype Overpower = Overpower SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overpower :: SkillCard Overpower
overpower = skill Overpower Cards.overpower

instance RunMessage env Overpower where
  runMessage msg s@(Overpower attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ push (DrawCards skillOwner 1 False)
    _ -> Overpower <$> runMessage msg attrs
