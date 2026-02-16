module Arkham.Skill.Cards.EsotericMethod1 (esotericMethod1) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.ChaosBag (getRemainingCurseTokens)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.SkillTestResult

newtype EsotericMethod1 = EsotericMethod1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericMethod1 :: SkillCard EsotericMethod1
esotericMethod1 = skill EsotericMethod1 Cards.esotericMethod1

instance RunMessage EsotericMethod1 where
  runMessage msg s@(EsotericMethod1 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy _ n | n > 0 -> do
              x <- getRemainingCurseTokens
              provideSkillTestResultOption attrs exclusions "Esoteric Method (1)" do
                addCurseTokens (Just st.investigator) (min x n)
            FailedBy _ n | n > 0 -> do
              x <- getRemainingCurseTokens
              provideSkillTestResultOption attrs exclusions "Esoteric Method (1)" do
                addCurseTokens (Just st.investigator) (min x n)
            _ -> pure ()
      pure s
    _ -> EsotericMethod1 <$> liftRunMessage msg attrs
