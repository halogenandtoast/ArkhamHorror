module Arkham.Types.Skill.Cards.UnexpectedCourage where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Skill.Attrs

newtype UnexpectedCourage = UnexpectedCourage SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedCourage :: SkillCard UnexpectedCourage
unexpectedCourage = skill UnexpectedCourage Cards.unexpectedCourage

instance RunMessage env UnexpectedCourage where
  runMessage msg (UnexpectedCourage attrs) =
    UnexpectedCourage <$> runMessage msg attrs
