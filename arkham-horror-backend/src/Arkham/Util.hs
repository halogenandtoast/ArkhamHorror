module Arkham.Util
  ( updateGame
  , without
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

without :: Int -> [a] -> [a]
without n as = [ a | (i, a) <- zip [0 ..] as, i /= n ]
