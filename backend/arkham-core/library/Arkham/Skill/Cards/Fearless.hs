module Arkham.Skill.Cards.Fearless where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Fearless = Fearless SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

fearless :: SkillCard Fearless
fearless = skill Fearless Cards.fearless

instance RunMessage Fearless where
  runMessage msg s@(Fearless attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      mHealHorror <- getHealHorrorMessage attrs 1 (skillOwner attrs)
      for_ mHealHorror push
      pure s
    _ -> Fearless <$> runMessage msg attrs
