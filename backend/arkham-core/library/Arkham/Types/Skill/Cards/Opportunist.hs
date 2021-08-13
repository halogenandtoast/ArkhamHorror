module Arkham.Types.Skill.Cards.Opportunist
  ( opportunist
  , Opportunist(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype Opportunist = Opportunist SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

opportunist :: SkillCard Opportunist
opportunist = skill Opportunist Cards.opportunist

instance SkillRunner env => RunMessage env Opportunist where
  runMessage msg s@(Opportunist attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ n | sid == skillId && n >= 3 ->
      s <$ push (ReturnToHand iid (SkillTarget skillId))
    _ -> Opportunist <$> runMessage msg attrs
