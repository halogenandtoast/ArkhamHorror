module Arkham.Types.Skill.Cards.InquiringMind
  ( inquiringMind
  , InquiringMind(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Skill.Attrs

newtype InquiringMind = InquiringMind SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inquiringMind :: SkillCard InquiringMind
inquiringMind = skill InquiringMind Cards.inquiringMind

instance RunMessage env InquiringMind where
  runMessage msg (InquiringMind attrs) = InquiringMind <$> runMessage msg attrs
