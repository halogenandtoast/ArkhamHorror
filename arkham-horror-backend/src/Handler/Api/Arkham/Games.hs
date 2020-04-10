module Handler.Api.Arkham.Games where

import Import
import Arkham.Types

postApiV1ArkhamGamesR :: Handler GameSettings
postApiV1ArkhamGamesR = requireCheckJsonBody
