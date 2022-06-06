module Arkham.Skill.Cards.Guts where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Attrs
import Arkham.Target

newtype Guts = Guts SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guts :: SkillCard Guts
guts = skill Guts Cards.guts

instance RunMessage Guts where
  runMessage msg s@(Guts attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ _ | sid == skillId ->
      s <$ push (DrawCards skillOwner 1 False)
    _ -> Guts <$> runMessage msg attrs
