module Arkham.Skill.Cards.Fearless2 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Fearless2 = Fearless2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fearless2 :: SkillCard Fearless2
fearless2 = skill Fearless2 Cards.fearless2

instance RunMessage Fearless2 where
  runMessage msg s@(Fearless2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ n | sid == skillId -> do
      mHealHorror <- getHealHorrorMessage
        attrs
        (if n >= 2 then 2 else 1)
        skillOwner
      for_ mHealHorror push
      pure s
    _ -> Fearless2 <$> runMessage msg attrs
