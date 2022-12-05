module Arkham.Scenario.Scenarios.TurnBackTime
  ( TurnBackTime(..)
  , turnBackTime
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Scenario
import Arkham.Scenario.Runner
import Arkham.Token

newtype TurnBackTime = TurnBackTime ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

turnBackTime :: Difficulty -> TurnBackTime
turnBackTime difficulty = scenario
  TurnBackTime
  "04344"
  "Turn Back Time"
  difficulty
  []

instance HasTokenValue TurnBackTime where
  getTokenValue iid tokenFace (TurnBackTime attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TurnBackTime where
  runMessage msg (TurnBackTime attrs) =
    TurnBackTime <$> runMessage msg attrs
