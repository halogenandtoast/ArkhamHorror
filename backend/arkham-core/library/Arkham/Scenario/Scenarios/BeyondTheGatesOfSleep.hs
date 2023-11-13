module Arkham.Scenario.Scenarios.BeyondTheGatesOfSleep (
  BeyondTheGatesOfSleep (..),
  beyondTheGatesOfSleep,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner

newtype BeyondTheGatesOfSleep = BeyondTheGatesOfSleep ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheGatesOfSleep :: Difficulty -> BeyondTheGatesOfSleep
beyondTheGatesOfSleep difficulty =
  scenario
    BeyondTheGatesOfSleep
    "06039"
    "Beyond the Gates of Sleep"
    difficulty
    []

instance HasChaosTokenValue BeyondTheGatesOfSleep where
  getChaosTokenValue iid tokenFace (BeyondTheGatesOfSleep attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage BeyondTheGatesOfSleep where
  runMessage msg s@(BeyondTheGatesOfSleep attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> BeyondTheGatesOfSleep <$> runMessage msg attrs
