module Arkham.Skill.Cards.Fearless where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Runner
import Arkham.Target

newtype Fearless = Fearless SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearless :: SkillCard Fearless
fearless = skill Fearless Cards.fearless

instance RunMessage Fearless where
  runMessage msg s@(Fearless attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ push (HealHorror (InvestigatorTarget skillOwner) 1)
    _ -> Fearless <$> runMessage msg attrs
