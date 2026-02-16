module Arkham.Skill.Cards.Providential2 (providential2) where

import {-# SOURCE #-} Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.ChaosBag (getRemainingBlessTokens)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (InvestigatorDamage)
import Arkham.SkillTestResult

newtype Providential2 = Providential2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

providential2 :: SkillCard Providential2
providential2 = skill Providential2 Cards.providential2

instance RunMessage Providential2 where
  runMessage msg s@(Providential2 attrs) = runQueueT $ case msg of
    CheckSkillTestResultOptions skillTestId exclusions -> do
      mst <- getSkillTest
      for_ mst \st -> do
        when (st.id == skillTestId && isTarget attrs st.target) do
          case st.result of
            SucceededBy {} -> do
              n <- getRemainingBlessTokens
              d <- field InvestigatorDamage attrs.owner
              h <- field InvestigatorHorror attrs.owner
              let x = min n (min d h)
              provideSkillTestResultOption attrs exclusions "Providential (2)" do
                repeated x $ addChaosToken #bless
            _ -> pure ()
      pure s
    _ -> Providential2 <$> liftRunMessage msg attrs
