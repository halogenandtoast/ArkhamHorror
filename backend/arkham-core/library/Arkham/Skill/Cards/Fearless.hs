module Arkham.Skill.Cards.Fearless where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Fearless = Fearless SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearless :: SkillCard Fearless
fearless = skill Fearless Cards.fearless

instance RunMessage Fearless where
  runMessage msg s@(Fearless attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      canHeal <- canHaveHorrorHealed attrs (skillOwner attrs)
      pushWhen canHeal $ HealHorror (toTarget $ skillOwner attrs) (toSource attrs) 1
      pure s
    _ -> Fearless <$> runMessage msg attrs
