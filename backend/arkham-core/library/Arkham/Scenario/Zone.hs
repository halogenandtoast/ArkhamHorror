module Arkham.Scenario.Zone where

import Arkham.Prelude

data ScenarioOutOfPlayZone
  = VoidZone
  | PursuitZone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
