module Arkham.Skill.Cards.Overpower2 (overpower2, Overpower2 (..)) where

import Arkham.Classes
import Arkham.Message
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Overpower2 = Overpower2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overpower2 :: SkillCard Overpower2
overpower2 = skill Overpower2 Cards.overpower2

instance RunMessage Overpower2 where
  runMessage msg s@(Overpower2 attrs) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ n | sid == skillId attrs -> do
      let amount = if n >= 2 then 2 else 1
      push $ drawCards (skillOwner attrs) attrs amount
      pure s
    _ -> Overpower2 <$> runMessage msg attrs
