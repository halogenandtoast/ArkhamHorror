module Arkham.Skill.Cards.SignumCrucis2 (signumCrucis2) where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.SkillTest
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype SignumCrucis2 = SignumCrucis2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signumCrucis2 :: SkillCard SignumCrucis2
signumCrucis2 = skill SignumCrucis2 Cards.signumCrucis2

instance RunMessage SignumCrucis2 where
  runMessage msg (SignumCrucis2 attrs) = runQueueT $ case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      whenJustM getSkillTest \skillTest -> do
        n <- getSkillTestDifficultyDifferenceFromBaseValue iid skillTest
        x <- min n <$> getRemainingBlessTokens
        repeated x $ addChaosToken #bless
      SignumCrucis2 <$> liftRunMessage msg attrs
    _ -> SignumCrucis2 <$> liftRunMessage msg attrs
