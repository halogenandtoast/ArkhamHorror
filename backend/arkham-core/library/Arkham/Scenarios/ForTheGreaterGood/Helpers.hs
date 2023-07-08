module Arkham.Scenarios.ForTheGreaterGood.Helpers where

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Key
import Arkham.Prelude
import Arkham.Scenario.Types

getRandomKey :: GameT ArkhamKey
getRandomKey = do
  ks <- setToList <$> scenarioField ScenarioSetAsideKeys
  case nonEmpty ks of
    Nothing -> error "called with no remaining keys"
    Just neKeys -> sample neKeys
