module Arkham.Skill.Cards.WhispersFromTheDeep
  ( whispersFromTheDeep
  , WhispersFromTheDeep(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype WhispersFromTheDeep = WhispersFromTheDeep SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whispersFromTheDeep :: SkillCard WhispersFromTheDeep
whispersFromTheDeep =
  skill WhispersFromTheDeep Cards.whispersFromTheDeep

instance RunMessage WhispersFromTheDeep where
  runMessage msg (WhispersFromTheDeep attrs) = WhispersFromTheDeep <$> runMessage msg attrs
