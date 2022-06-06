module Arkham.Helpers.ChaosBag where

import Arkham.ChaosBag.Base
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Scenario.Attrs ( Field (..) )
import Arkham.Token

getTokensInBag :: GameT [Token]
getTokensInBag = scenarioFieldMap ScenarioChaosBag chaosBagTokens
