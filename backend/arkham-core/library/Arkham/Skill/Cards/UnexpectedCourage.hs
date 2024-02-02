module Arkham.Skill.Cards.UnexpectedCourage where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype UnexpectedCourage = UnexpectedCourage SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

unexpectedCourage :: SkillCard UnexpectedCourage
unexpectedCourage = skill UnexpectedCourage Cards.unexpectedCourage

instance RunMessage UnexpectedCourage where
  runMessage msg (UnexpectedCourage attrs) = UnexpectedCourage <$> runMessage msg attrs
