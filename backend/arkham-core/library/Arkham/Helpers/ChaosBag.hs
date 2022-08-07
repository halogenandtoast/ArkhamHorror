module Arkham.Helpers.ChaosBag where

import Arkham.Prelude

import Arkham.ChaosBag.Base
import Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types ( Field (..) )
import Arkham.Token

getTokensInBag :: (Monad m, HasGame m) => m [Token]
getTokensInBag = scenarioFieldMap ScenarioChaosBag chaosBagTokens
