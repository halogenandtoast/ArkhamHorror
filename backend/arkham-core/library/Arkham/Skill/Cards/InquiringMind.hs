module Arkham.Skill.Cards.InquiringMind (
  inquiringMind,
  InquiringMind (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype InquiringMind = InquiringMind SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

inquiringMind :: SkillCard InquiringMind
inquiringMind = skill InquiringMind Cards.inquiringMind

instance RunMessage InquiringMind where
  runMessage msg (InquiringMind attrs) = InquiringMind <$> runMessage msg attrs
