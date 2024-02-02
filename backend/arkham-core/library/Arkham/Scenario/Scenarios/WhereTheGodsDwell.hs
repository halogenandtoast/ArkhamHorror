module Arkham.Scenario.Scenarios.WhereTheGodsDwell (
  WhereTheGodsDwell (..),
  whereTheGodsDwell,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner

newtype WhereTheGodsDwell = WhereTheGodsDwell ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

whereTheGodsDwell :: Difficulty -> WhereTheGodsDwell
whereTheGodsDwell difficulty =
  scenario
    WhereTheGodsDwell
    "06286"
    "Where the Gods Dwell"
    difficulty
    []

instance HasChaosTokenValue WhereTheGodsDwell where
  getChaosTokenValue iid tokenFace (WhereTheGodsDwell attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WhereTheGodsDwell where
  runMessage msg s@(WhereTheGodsDwell attrs) = case msg of
    Setup -> do
      push $ EndOfGame Nothing
      pure s
    _ -> WhereTheGodsDwell <$> runMessage msg attrs
