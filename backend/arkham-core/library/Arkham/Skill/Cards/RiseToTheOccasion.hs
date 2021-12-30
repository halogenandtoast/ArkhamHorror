module Arkham.Skill.Cards.RiseToTheOccasion
  ( riseToTheOccasion
  , RiseToTheOccasion(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Classes
import Arkham.Skill.Attrs

newtype RiseToTheOccasion = RiseToTheOccasion SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseToTheOccasion :: SkillCard RiseToTheOccasion
riseToTheOccasion = skill RiseToTheOccasion Cards.riseToTheOccasion

instance RunMessage env RiseToTheOccasion where
  runMessage msg (RiseToTheOccasion attrs) =
    RiseToTheOccasion <$> runMessage msg attrs
