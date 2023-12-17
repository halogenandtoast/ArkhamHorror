module Arkham.Scenario.Scenarios.TheSearchForKadath (TheSearchForKadath (..), theSearchForKadath) where

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Prelude
import Arkham.Scenario.Runner

newtype TheSearchForKadath = TheSearchForKadath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForKadath :: Difficulty -> TheSearchForKadath
theSearchForKadath difficulty =
  scenario
    TheSearchForKadath
    "06119"
    "The Search for Kadath"
    difficulty
    ["ulthar skaiRiver dylathLeen"]

instance HasChaosTokenValue TheSearchForKadath where
  getChaosTokenValue iid tokenFace (TheSearchForKadath attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheSearchForKadath where
  runMessage msg s@(TheSearchForKadath attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> TheSearchForKadath <$> runMessage msg attrs
