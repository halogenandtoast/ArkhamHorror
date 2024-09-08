module Arkham.Scenario.Scenarios.ReturnToExtracurricularActivity (
  ReturnToExtracurricularActivity (..),
  returnToExtracurricularActivity,
) where

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Prelude
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios.ExtracurricularActivity
import Arkham.Scenario.Setup

newtype ReturnToExtracurricularActivity = ReturnToExtracurricularActivity ExtracurricularActivity
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToExtracurricularActivity :: Difficulty -> ReturnToExtracurricularActivity
returnToExtracurricularActivity =
  returnTo
    ReturnToExtracurricularActivity
    "51012"
    "Return to Extracurricular Activity"
    extracurricularActivity

instance HasChaosTokenValue ReturnToExtracurricularActivity where
  getChaosTokenValue iid tokenFace (ReturnToExtracurricularActivity (ExtracurricularActivity attrs)) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ReturnToExtracurricularActivity where
  runMessage msg (ReturnToExtracurricularActivity base@(ExtracurricularActivity attrs)) = case msg of
    Setup -> runScenarioSetup (ReturnToExtracurricularActivity . ExtracurricularActivity) attrs do
      pure ()
    _ -> ReturnToExtracurricularActivity <$> runMessage msg base
