module Arkham.Api.Handler.Games
  ( getApiV1ArkhamGameR
  )
where

import Arkham.Types
import Arkham.Types.Game
import Import

getApiV1ArkhamGameR :: ArkhamGameId -> Handler ArkhamGameData
getApiV1ArkhamGameR = runDB . (arkhamGameCurrentData <$>) . get404
