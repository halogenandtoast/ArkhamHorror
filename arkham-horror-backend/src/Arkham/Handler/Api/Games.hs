module Arkham.Handler.Api.Games where

import Import

getApiV1ArkhamGameR :: ArkhamHorrorGameId -> Handler Int
getApiV1ArkhamGameR _ = pure 1
