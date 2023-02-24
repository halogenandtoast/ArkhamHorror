module Arkham.Skill.Cards.Opportunist2
  ( opportunist2
  , Opportunist2(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Skill.Runner

newtype Opportunist2 = Opportunist2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opportunist2 :: SkillCard Opportunist2
opportunist2 = skill Opportunist2 Cards.opportunist2

instance RunMessage Opportunist2 where
  runMessage msg s@(Opportunist2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ n | sid == skillId && n >= 2 ->
      s <$ push (ReturnToHand iid (SkillTarget skillId))
    _ -> Opportunist2 <$> runMessage msg attrs
