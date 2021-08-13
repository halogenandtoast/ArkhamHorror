module Arkham.Types.Skill.Cards.Opportunist2
  ( opportunist2
  , Opportunist2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype Opportunist2 = Opportunist2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opportunist2 :: SkillCard Opportunist2
opportunist2 = skill Opportunist2 Cards.opportunist2

instance SkillRunner env => RunMessage env Opportunist2 where
  runMessage msg s@(Opportunist2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ n | sid == skillId && n >= 2 ->
      s <$ push (ReturnToHand iid (SkillTarget skillId))
    _ -> Opportunist2 <$> runMessage msg attrs
