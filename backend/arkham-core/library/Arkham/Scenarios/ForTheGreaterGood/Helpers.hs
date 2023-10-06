module Arkham.Scenarios.ForTheGreaterGood.Helpers where

import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.Key
import Arkham.Prelude
import Arkham.Scenario.Types

getRandomKey :: (HasGame m, MonadRandom m) => m (Maybe ArkhamKey)
getRandomKey = do
  ks <- setToList <$> scenarioField ScenarioSetAsideKeys
  traverse sample $ nonEmpty ks
