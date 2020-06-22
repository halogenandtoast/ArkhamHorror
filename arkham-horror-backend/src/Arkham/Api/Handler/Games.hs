module Arkham.Api.Handler.Games where

import Import
import Arkham.Types
import Arkham.Fixtures

getApiV1ArkhamGameR :: ArkhamGameId -> Handler ArkhamGame
getApiV1ArkhamGameR = runDB . getBy404
