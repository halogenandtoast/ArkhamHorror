module Arkham.Skill.Cards.SealOfTheElderSign5 (sealOfTheElderSign5) where

import Arkham.ChaosToken
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Strategy

newtype SealOfTheElderSign5 = SealOfTheElderSign5 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor SealOfTheElderSign5 where
  getModifiersFor (SealOfTheElderSign5 attrs) = do
    getSkillTest >>= traverse_ \st -> do
      modified_
        attrs
        st.investigator
        [DoNotDrawChaosTokensForSkillChecks, TreatRevealedChaosTokenAs ElderSign]

sealOfTheElderSign5 :: SkillCard SealOfTheElderSign5
sealOfTheElderSign5 =
  skillWith SealOfTheElderSign5 Cards.sealOfTheElderSign5 $ afterPlayL .~ RemoveThisFromGame

instance RunMessage SealOfTheElderSign5 where
  runMessage msg (SealOfTheElderSign5 attrs) =
    SealOfTheElderSign5 <$> runMessage msg attrs
