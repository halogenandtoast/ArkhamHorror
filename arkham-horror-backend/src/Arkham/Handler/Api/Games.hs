module Arkham.Handler.Api.Games where

import Import
import Arkham.Types

postApiV1ArkhamGamesR :: Handler GameSettings
postApiV1ArkhamGamesR = do
  void requireAuthId
  requireCheckJsonBody
