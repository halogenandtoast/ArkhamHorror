module Arkham.Skill.Cards.InquiringMind (inquiringMind) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype InquiringMind = InquiringMind SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inquiringMind :: SkillCard InquiringMind
inquiringMind = skill InquiringMind Cards.inquiringMind

instance RunMessage InquiringMind where
  runMessage msg (InquiringMind attrs) = InquiringMind <$> runMessage msg attrs
