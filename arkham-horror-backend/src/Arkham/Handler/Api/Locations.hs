module Arkham.Handler.Api.Locations where

import Import
import Arkham.Types

postApiV1ArkhamGameLocationsRevealR :: ArkhamHorrorGameId -> Handler ArkhamLocation
postApiV1ArkhamGameLocationsRevealR _ = pure $ ArkhamLocationRevealed $
  ArkhamLocationRevealedData
    { arkhamLocationRevealedDataName = "Study"
    , arkhamLocationRevealedDataSymbol = ArkhamLocationSymbolCircle
    , arkhamLocationRevealedDataConnections = []
    , arkhamLocationRevealedDataShroud = 2
    , arkhamLocationRevealedDataMaxClues = ArkhamClueCountPerInvestigator 2
    , arkhamLocationRevealedDataCurrentClues = 2
    , arkhamLocationRevealedDataImageUrl = "https://arkhamdb.com/bundles/cards/01111.png"
    }
  
