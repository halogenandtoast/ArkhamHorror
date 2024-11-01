module Arkham.Skill.Cards.NeitherRainNorSnow (neitherRainNorSnow, NeitherRainNorSnow (..)) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype NeitherRainNorSnow = NeitherRainNorSnow SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

neitherRainNorSnow :: SkillCard NeitherRainNorSnow
neitherRainNorSnow = skill NeitherRainNorSnow Cards.neitherRainNorSnow

instance RunMessage NeitherRainNorSnow where
  runMessage msg s@(NeitherRainNorSnow attrs) = runQueueT $ case msg of
    When (FailedSkillTest _ _ _ target _ _) | isTarget attrs target -> do
      cancelSkillTestEffects attrs
      pure s
    _ -> NeitherRainNorSnow <$> liftRunMessage msg attrs
