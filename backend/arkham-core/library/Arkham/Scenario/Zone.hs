module Arkham.Scenario.Zone where

import Arkham.Prelude

data ScenarioZone
  = VoidZone
  | PursuitZone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
