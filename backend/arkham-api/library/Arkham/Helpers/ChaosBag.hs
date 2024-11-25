module Arkham.Helpers.ChaosBag where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.ChaosBag.Base
import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))

getOnlyChaosTokensInBag :: HasGame m => m [ChaosToken]
getOnlyChaosTokensInBag = scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens

getBagChaosTokens :: HasGame m => m [ChaosToken]
getBagChaosTokens = scenarioFieldMap ScenarioChaosBag allChaosBagChaosTokens

getTokenPool :: HasGame m => m [ChaosToken]
getTokenPool = scenarioFieldMap ScenarioChaosBag chaosBagTokenPool

getRemainingFrostTokens :: HasGame m => m Int
getRemainingFrostTokens = selectCount $ InTokenPool $ ChaosTokenFaceIs FrostToken

hasRemainingFrostTokens :: HasGame m => m Bool
hasRemainingFrostTokens = (> 0) <$> getRemainingFrostTokens

getRemainingCurseTokens :: HasGame m => m Int
getRemainingCurseTokens = selectCount $ InTokenPool $ ChaosTokenFaceIs CurseToken

getRemainingBlessTokens :: HasGame m => m Int
getRemainingBlessTokens = selectCount $ InTokenPool $ ChaosTokenFaceIs BlessToken

getSealedChaosTokens :: HasGame m => m [ChaosToken]
getSealedChaosTokens =
  concat
    <$> sequence
      [ selectAgg id AssetSealedChaosTokens AnyAsset
      , selectAgg id EnemySealedChaosTokens AnyInPlayEnemy
      , selectAgg id EventSealedChaosTokens AnyEvent
      , selectAgg id InvestigatorSealedChaosTokens Anyone
      ]

getAllChaosTokens :: HasGame m => m [ChaosToken]
getAllChaosTokens = nub . concat <$> sequence [getBagChaosTokens, getSealedChaosTokens]

getChaosBagChoice :: HasGame m => m (Maybe ChaosBagStepState)
getChaosBagChoice = scenarioFieldMap ScenarioChaosBag chaosBagChoice

getChaosBag :: HasGame m => m ChaosBag
getChaosBag = scenarioField ScenarioChaosBag
