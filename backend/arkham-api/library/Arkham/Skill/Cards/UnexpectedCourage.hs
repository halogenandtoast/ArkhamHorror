module Arkham.Skill.Cards.UnexpectedCourage (unexpectedCourage) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype UnexpectedCourage = UnexpectedCourage SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedCourage :: SkillCard UnexpectedCourage
unexpectedCourage = skill UnexpectedCourage Cards.unexpectedCourage

instance RunMessage UnexpectedCourage where
  runMessage msg (UnexpectedCourage attrs) = UnexpectedCourage <$> runMessage msg attrs
