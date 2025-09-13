module Arkham.Skill.Cards.RiseToTheOccasion3 (riseToTheOccasion3) where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype RiseToTheOccasion3 = RiseToTheOccasion3 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseToTheOccasion3 :: SkillCard RiseToTheOccasion3
riseToTheOccasion3 = skill RiseToTheOccasion3 Cards.riseToTheOccasion3

instance HasModifiersFor RiseToTheOccasion3 where
  getModifiersFor (RiseToTheOccasion3 attrs) = do
    whenJustM getSkillTest \skillTest -> do
      base <- getSkillTestBaseSkillForSkillTest attrs.owner skillTest
      difficulty <- getModifiedSkillTestDifficulty skillTest
      let n = max 0 (min 3 (difficulty - base))
      addSkillIcons attrs $ replicate n #wild

instance RunMessage RiseToTheOccasion3 where
  runMessage msg (RiseToTheOccasion3 attrs) =
    RiseToTheOccasion3 <$> runMessage msg attrs
