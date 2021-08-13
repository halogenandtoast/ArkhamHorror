module Arkham.Types.Skill.Cards.RiseToTheOccasion
  ( riseToTheOccasion
  , RiseToTheOccasion(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype RiseToTheOccasion = RiseToTheOccasion SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseToTheOccasion :: SkillCard RiseToTheOccasion
riseToTheOccasion = skill RiseToTheOccasion Cards.riseToTheOccasion

instance SkillRunner env => RunMessage env RiseToTheOccasion where
  runMessage msg (RiseToTheOccasion attrs) =
    RiseToTheOccasion <$> runMessage msg attrs
