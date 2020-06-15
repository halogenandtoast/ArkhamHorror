module Arkham.Api.Handler.Games where

import Import
import Arkham.Types
import Arkham.Fixtures

getApiV1ArkhamGameR :: Int -> Handler ArkhamGame
getApiV1ArkhamGameR = liftIO . loadGameFixture
