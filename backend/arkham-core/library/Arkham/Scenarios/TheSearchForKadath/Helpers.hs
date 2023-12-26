module Arkham.Scenarios.TheSearchForKadath.Helpers where

import Arkham.Classes.HasGame
import Arkham.Helpers.Log
import Arkham.Prelude
import Arkham.ScenarioLogKey

getSignsOfTheGods :: HasGame m => m Int
getSignsOfTheGods = scenarioCount SignOfTheGods
