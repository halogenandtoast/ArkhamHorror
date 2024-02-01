module Arkham.Skill.Cards.RiseToTheOccasion (
  riseToTheOccasion,
  RiseToTheOccasion (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype RiseToTheOccasion = RiseToTheOccasion SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

riseToTheOccasion :: SkillCard RiseToTheOccasion
riseToTheOccasion = skill RiseToTheOccasion Cards.riseToTheOccasion

instance RunMessage RiseToTheOccasion where
  runMessage msg (RiseToTheOccasion attrs) =
    RiseToTheOccasion <$> runMessage msg attrs
