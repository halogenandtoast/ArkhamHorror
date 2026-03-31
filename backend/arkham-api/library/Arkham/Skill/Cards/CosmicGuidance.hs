module Arkham.Skill.Cards.CosmicGuidance (cosmicGuidance) where

import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype CosmicGuidance = CosmicGuidance SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicGuidance :: SkillCard CosmicGuidance
cosmicGuidance = skill CosmicGuidance Cards.cosmicGuidance

instance RunMessage CosmicGuidance where
  runMessage msg s@(CosmicGuidance attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      healDamage iid attrs 1
      pure s
    _ -> CosmicGuidance <$> liftRunMessage msg attrs
