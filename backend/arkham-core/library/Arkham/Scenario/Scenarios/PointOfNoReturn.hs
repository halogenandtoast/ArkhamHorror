module Arkham.Scenario.Scenarios.PointOfNoReturn (
  PointOfNoReturn (..),
  pointOfNoReturn,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner

newtype PointOfNoReturn = PointOfNoReturn ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

pointOfNoReturn :: Difficulty -> PointOfNoReturn
pointOfNoReturn difficulty =
  scenario
    PointOfNoReturn
    "06247"
    "Point of No Return"
    difficulty
    []

instance HasChaosTokenValue PointOfNoReturn where
  getChaosTokenValue iid tokenFace (PointOfNoReturn attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage PointOfNoReturn where
  runMessage msg s@(PointOfNoReturn attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> PointOfNoReturn <$> runMessage msg attrs
