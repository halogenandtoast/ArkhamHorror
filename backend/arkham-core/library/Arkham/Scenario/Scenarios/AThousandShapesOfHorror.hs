module Arkham.Scenario.Scenarios.AThousandShapesOfHorror (
  AThousandShapesOfHorror (..),
  aThousandShapesOfHorror,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner

newtype AThousandShapesOfHorror = AThousandShapesOfHorror ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aThousandShapesOfHorror :: Difficulty -> AThousandShapesOfHorror
aThousandShapesOfHorror difficulty =
  scenario
    AThousandShapesOfHorror
    "06168"
    "A Thousand Shapes of Horror"
    difficulty
    []

instance HasChaosTokenValue AThousandShapesOfHorror where
  getChaosTokenValue iid tokenFace (AThousandShapesOfHorror attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AThousandShapesOfHorror where
  runMessage msg s@(AThousandShapesOfHorror attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> AThousandShapesOfHorror <$> runMessage msg attrs
