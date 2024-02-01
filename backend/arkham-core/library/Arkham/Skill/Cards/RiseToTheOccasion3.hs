module Arkham.Skill.Cards.RiseToTheOccasion3 (
  riseToTheOccasion3,
  RiseToTheOccasion3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype RiseToTheOccasion3 = RiseToTheOccasion3 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

riseToTheOccasion3 :: SkillCard RiseToTheOccasion3
riseToTheOccasion3 = skill RiseToTheOccasion3 Cards.riseToTheOccasion3

instance HasModifiersFor RiseToTheOccasion3 where
  getModifiersFor target (RiseToTheOccasion3 attrs) | attrs `isTarget` target = do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Just skillTest -> do
        base <- getSkillTestBaseSkillForSkillTest (skillOwner attrs) skillTest
        difficulty <- getModifiedSkillTestDifficulty skillTest
        let n = max 0 (min 3 (difficulty - base))
        pure $ toModifiers attrs [AddSkillIcons $ replicate n #wild]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage RiseToTheOccasion3 where
  runMessage msg (RiseToTheOccasion3 attrs) =
    RiseToTheOccasion3 <$> runMessage msg attrs
