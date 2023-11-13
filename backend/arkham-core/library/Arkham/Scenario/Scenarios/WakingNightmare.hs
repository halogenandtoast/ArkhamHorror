module Arkham.Scenario.Scenarios.WakingNightmare (
  WakingNightmare (..),
  wakingNightmare,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner

newtype WakingNightmare = WakingNightmare ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wakingNightmare :: Difficulty -> WakingNightmare
wakingNightmare difficulty =
  scenario
    WakingNightmare
    "06063"
    "Waking Nightmare"
    difficulty
    []

instance HasChaosTokenValue WakingNightmare where
  getChaosTokenValue iid tokenFace (WakingNightmare attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WakingNightmare where
  runMessage msg s@(WakingNightmare attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> WakingNightmare <$> runMessage msg attrs
