module Arkham.Skill.Cards.NauticalProwess
  ( nauticalProwess
  , NauticalProwess(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype NauticalProwess = NauticalProwess SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nauticalProwess :: SkillCard NauticalProwess
nauticalProwess =
  skill NauticalProwess Cards.nauticalProwess

instance RunMessage NauticalProwess where
  runMessage msg (NauticalProwess attrs) = NauticalProwess <$> runMessage msg attrs
