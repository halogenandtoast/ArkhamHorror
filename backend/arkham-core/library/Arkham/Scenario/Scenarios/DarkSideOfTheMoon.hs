module Arkham.Scenario.Scenarios.DarkSideOfTheMoon (
  DarkSideOfTheMoon (..),
  darkSideOfTheMoon,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner

newtype DarkSideOfTheMoon = DarkSideOfTheMoon ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

darkSideOfTheMoon :: Difficulty -> DarkSideOfTheMoon
darkSideOfTheMoon difficulty =
  scenario
    DarkSideOfTheMoon
    "06206"
    "Dark Side of the Moon"
    difficulty
    []

instance HasChaosTokenValue DarkSideOfTheMoon where
  getChaosTokenValue iid tokenFace (DarkSideOfTheMoon attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DarkSideOfTheMoon where
  runMessage msg s@(DarkSideOfTheMoon attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> DarkSideOfTheMoon <$> runMessage msg attrs
