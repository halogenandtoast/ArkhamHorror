module Arkham.Api.Handler.Turns where

import Arkham.Types
import Arkham.Util
import Import
import Lens.Micro

postApiV1ArkhamGameEndTurnR :: ArkhamGameId -> Handler ArkhamGameData
postApiV1ArkhamGameEndTurnR gameId = do
  g <- runDB $ get404 gameId
  runDB $ updateGame gameId $ g & player . endedTurn .~ True
