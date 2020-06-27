module Arkham.Api.Handler.Turns where

import Arkham.Types
import Arkham.Util
import Import
import Lens.Micro

-- brittany-disable-next-binding
runGamePhase :: ArkhamGame -> ArkhamGame
runGamePhase g = case g ^. phase of
  Investigation -> g
  Enemy -> runGamePhase $ g & phase .~ Upkeep
  Upkeep ->
    runGamePhase
      $ g & currentData %~ drawCard
          & player . resources +~ 1
          & phase .~ Mythos
  Mythos -> g & phase .~ Investigation


postApiV1ArkhamGameEndTurnR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameEndTurnR gameId = do
  g <- runDB $ get404 gameId
  runDB $ updateGame gameId $ runGamePhase $ g & phase .~ Enemy
