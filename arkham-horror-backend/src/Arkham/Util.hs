module Arkham.Util
  ( updateGame
  )
where

import Arkham.Entity.ArkhamGame
import Arkham.Internal.Scenario
import Arkham.Internal.Types
import Arkham.Types.Game
import ClassyPrelude
import Database.Persist.Sql

updateGame
  :: (MonadIO m) => ArkhamGameId -> ArkhamGame -> SqlPersistT m ArkhamGameData
updateGame gameId game = do
  let scenario' = toInternalScenario game
  updatedGame <- liftIO $ scenarioRun scenario' game
  replace gameId updatedGame $> arkhamGameCurrentData updatedGame
