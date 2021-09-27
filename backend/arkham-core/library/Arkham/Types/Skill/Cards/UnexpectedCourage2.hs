module Arkham.Types.Skill.Cards.UnexpectedCourage2
  ( unexpectedCourage2
  , UnexpectedCourage2(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype UnexpectedCourage2 = UnexpectedCourage2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedCourage2 :: SkillCard UnexpectedCourage2
unexpectedCourage2 = skill UnexpectedCourage2 Cards.unexpectedCourage2

instance SkillRunner env => RunMessage env UnexpectedCourage2 where
  runMessage msg s@(UnexpectedCourage2 attrs) = case msg of
    FailedSkillTest iid _ _ target _ _ | isTarget attrs target ->
      s <$ push (ReturnToHand iid target)
    _ -> UnexpectedCourage2 <$> runMessage msg attrs
