module Arkham.Api.Handler.Games where

import Arkham.Types
import Import

getApiV1ArkhamGameR :: ArkhamGameId -> Handler ArkhamGameData
getApiV1ArkhamGameR = runDB . (arkhamGameCurrentData <$>) . get404
