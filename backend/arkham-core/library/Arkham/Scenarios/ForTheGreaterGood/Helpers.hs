module Arkham.Scenarios.ForTheGreaterGood.Helpers where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Key
import Arkham.Prelude
import Arkham.Scenario.Types

getRandomKey :: GameT (Maybe ArkhamKey)
getRandomKey = do
  ks <- setToList <$> scenarioField ScenarioSetAsideKeys
  traverse sample $ nonEmpty ks
