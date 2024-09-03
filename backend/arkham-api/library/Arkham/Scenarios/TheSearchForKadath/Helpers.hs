module Arkham.Scenarios.TheSearchForKadath.Helpers where

import Arkham.Classes.HasGame
import Arkham.Helpers.Log
import Arkham.Prelude
import Arkham.ScenarioLogKey

getSignsOfTheGods :: HasGame m => m Int
getSignsOfTheGods = scenarioCount SignOfTheGods

data Region = Oriab | Mnar | ForbiddenLands | TimelessRealm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Meta = Meta {regions :: [Region]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
