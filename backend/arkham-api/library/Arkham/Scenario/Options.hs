module Arkham.Scenario.Options where

import Arkham.Prelude

data ScenarioOptions = ScenarioOptions
  { scenarioOptionsStandalone :: Bool
  , scenarioOptionsPerformTarotReading :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

