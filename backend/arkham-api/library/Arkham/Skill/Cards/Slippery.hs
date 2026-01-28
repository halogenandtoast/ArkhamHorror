module Arkham.Skill.Cards.Slippery (slippery) where

import Arkham.Helpers.SkillTest (isEvading, withSkillTestTargetedEnemy)
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Slippery = Slippery SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slippery :: SkillCard Slippery
slippery = skill Slippery Cards.slippery

instance RunMessage Slippery where
  runMessage msg s@(Slippery attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      withSkillTestTargetedEnemy \enemy ->
        whenM (isEvading enemy) do
          nextPhaseModifier #upkeep attrs enemy DoesNotReadyDuringUpkeep
      pure s
    _ -> Slippery <$> liftRunMessage msg attrs
