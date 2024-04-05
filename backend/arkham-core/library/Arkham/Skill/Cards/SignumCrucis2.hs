module Arkham.Skill.Cards.SignumCrucis2 (signumCrucis2, SignumCrucis2 (..)) where

import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.SkillTest
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SignumCrucis2 = SignumCrucis2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signumCrucis2 :: SkillCard SignumCrucis2
signumCrucis2 = skill SignumCrucis2 Cards.signumCrucis2

instance RunMessage SignumCrucis2 where
  runMessage msg s@(SignumCrucis2 attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      getSkillTest >>= \case
        Nothing -> error "no skill test"
        Just skillTest -> do
          n <- getSkillTestDifficultyDifferenceFromBaseValue iid skillTest
          x <- min n <$> getRemainingBlessTokens
          pushAll $ replicate x $ AddChaosToken #bless
      pure s
    _ -> SignumCrucis2 <$> runMessage msg attrs
