module Arkham.Skill.Cards.RiseToTheOccasion (riseToTheOccasion) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype RiseToTheOccasion = RiseToTheOccasion SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseToTheOccasion :: SkillCard RiseToTheOccasion
riseToTheOccasion = skill RiseToTheOccasion Cards.riseToTheOccasion

instance RunMessage RiseToTheOccasion where
  runMessage msg (RiseToTheOccasion attrs) = RiseToTheOccasion <$> runMessage msg attrs
