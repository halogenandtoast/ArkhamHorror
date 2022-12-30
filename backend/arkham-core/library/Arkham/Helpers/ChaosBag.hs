module Arkham.Helpers.ChaosBag where

import Arkham.Prelude

import Arkham.Asset.Types ( Field (..) )
import Arkham.ChaosBag.Base
import Arkham.Classes.Query
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Event.Types ( Field (..) )
import Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Scenario.Types ( Field (..) )
import Arkham.Token

getOnlyTokensInBag :: HasGame m => m [Token]
getOnlyTokensInBag = scenarioFieldMap ScenarioChaosBag chaosBagTokens

getBagTokens :: HasGame m => m [Token]
getBagTokens = scenarioFieldMap ScenarioChaosBag allChaosBagTokens

getSealedTokens :: HasGame m => m [Token]
getSealedTokens = concat <$> sequence
  [ selectAgg id AssetSealedTokens AnyAsset
  , selectAgg id EnemySealedTokens AnyEnemy
  , selectAgg id EventSealedTokens AnyEvent
  ]

getAllTokens :: HasGame m => m [Token]
getAllTokens = concat <$> sequence [getBagTokens, getSealedTokens]
