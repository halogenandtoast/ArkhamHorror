module Arkham.Skill.Cards.EsotericMethod1 (esotericMethod1, EsotericMethod1 (..)) where

import Arkham.Helpers.ChaosBag (getRemainingCurseTokens)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype EsotericMethod1 = EsotericMethod1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericMethod1 :: SkillCard EsotericMethod1
esotericMethod1 = skill EsotericMethod1 Cards.esotericMethod1

instance RunMessage EsotericMethod1 where
  runMessage msg s@(EsotericMethod1 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ n | n > 0 -> do
      x <- getRemainingCurseTokens
      replicateM_ (min x n) $ push $ AddChaosToken #curse
      pure s
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ n | n > 0 -> do
      x <- getRemainingCurseTokens
      replicateM_ (min x n) $ push $ AddChaosToken #curse
      pure s
    _ -> EsotericMethod1 <$> liftRunMessage msg attrs
