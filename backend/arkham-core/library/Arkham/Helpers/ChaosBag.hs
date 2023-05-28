module Arkham.Helpers.ChaosBag where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Store
import Arkham.Token

getOnlyTokensInBag :: (HasGame m, Store m Card) => m [Token]
getOnlyTokensInBag = scenarioFieldMap ScenarioChaosBag chaosBagTokens

getBagTokens :: (HasGame m, Store m Card) => m [Token]
getBagTokens = scenarioFieldMap ScenarioChaosBag allChaosBagTokens

getSealedTokens :: (HasGame m, Store m Card) => m [Token]
getSealedTokens =
  concat
    <$> sequence
      [ selectAgg id AssetSealedTokens AnyAsset
      , selectAgg id EnemySealedTokens AnyEnemy
      , selectAgg id EventSealedTokens AnyEvent
      ]

getAllTokens :: (HasGame m, Store m Card) => m [Token]
getAllTokens = concat <$> sequence [getBagTokens, getSealedTokens]
