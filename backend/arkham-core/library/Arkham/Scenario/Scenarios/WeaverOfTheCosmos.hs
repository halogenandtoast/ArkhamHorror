module Arkham.Scenario.Scenarios.WeaverOfTheCosmos (
  WeaverOfTheCosmos (..),
  weaverOfTheCosmos,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner

newtype WeaverOfTheCosmos = WeaverOfTheCosmos ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

weaverOfTheCosmos :: Difficulty -> WeaverOfTheCosmos
weaverOfTheCosmos difficulty =
  scenario
    WeaverOfTheCosmos
    "06333"
    "Weaver of the Cosmos"
    difficulty
    []

instance HasChaosTokenValue WeaverOfTheCosmos where
  getChaosTokenValue iid tokenFace (WeaverOfTheCosmos attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WeaverOfTheCosmos where
  runMessage msg s@(WeaverOfTheCosmos attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> WeaverOfTheCosmos <$> runMessage msg attrs
