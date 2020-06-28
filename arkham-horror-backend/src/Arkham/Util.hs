module Arkham.Util where

import Arkham.Internal.Scenario
import Arkham.Internal.Types
import Arkham.Types
import ClassyPrelude
import Database.Persist.Sql

updateGame
  :: (MonadIO m) => ArkhamGameId -> ArkhamGame -> SqlPersistT m ArkhamGameData
updateGame gameId game = replace gameId updatedGame
  $> arkhamGameCurrentData updatedGame
 where
  scenario' = toInternalScenario game
  updatedGame = scenarioRun scenario' game
