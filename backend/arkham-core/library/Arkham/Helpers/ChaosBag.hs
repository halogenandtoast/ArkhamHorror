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

getTokensInBag :: (Monad m, HasGame m) => m [Token]
getTokensInBag = scenarioFieldMap ScenarioChaosBag chaosBagTokens

getSealedTokens :: (Monad m, HasGame m) => m [Token]
getSealedTokens = concat <$> sequence
  [ selectAgg id AssetSealedTokens AnyAsset
  , selectAgg id EnemySealedTokens AnyEnemy
  , selectAgg id EventSealedTokens AnyEvent
  ]

getAllTokens :: (Monad m, HasGame m) => m [Token]
getAllTokens = concat <$> sequence [getTokensInBag, getSealedTokens]
