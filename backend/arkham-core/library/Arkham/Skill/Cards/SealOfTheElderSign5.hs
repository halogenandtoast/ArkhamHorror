module Arkham.Skill.Cards.SealOfTheElderSign5 (
  sealOfTheElderSign5,
  SealOfTheElderSign5 (..),
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SealOfTheElderSign5 = SealOfTheElderSign5 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

instance HasModifiersFor SealOfTheElderSign5 where
  getModifiersFor (InvestigatorTarget _) (SealOfTheElderSign5 attrs) = do
    mSkillTest <- getSkillTest
    case mSkillTest of
      Just _ ->
        pure $ toModifiers attrs [DoNotDrawChaosTokensForSkillChecks, TreatRevealedChaosTokenAs ElderSign]
      Nothing -> pure []
  getModifiersFor _ _ = pure []

sealOfTheElderSign5 :: SkillCard SealOfTheElderSign5
sealOfTheElderSign5 =
  skillWith SealOfTheElderSign5 Cards.sealOfTheElderSign5
    $ afterPlayL
    .~ RemoveThisFromGame

instance RunMessage SealOfTheElderSign5 where
  runMessage msg (SealOfTheElderSign5 attrs) =
    SealOfTheElderSign5 <$> runMessage msg attrs
