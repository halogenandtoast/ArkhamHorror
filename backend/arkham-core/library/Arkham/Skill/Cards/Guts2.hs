module Arkham.Skill.Cards.Guts2 (
  guts2,
  Guts2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Guts2 = Guts2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

guts2 :: SkillCard Guts2
guts2 = skill Guts2 Cards.guts2

instance RunMessage Guts2 where
  runMessage msg s@(Guts2 attrs) = case msg of
    PassedSkillTest _ _ _ (SkillTarget sid) _ n | sid == skillId attrs -> do
      let amount = if n >= 2 then 2 else 1
      drawing <- drawCards (skillOwner attrs) attrs amount
      push drawing
      pure s
    _ -> Guts2 <$> runMessage msg attrs
