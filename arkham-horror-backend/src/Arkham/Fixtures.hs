module Arkham.Fixtures where

import Arkham.Types
import Prelude (($))


rolandBanks :: ArkhamInvestigator
rolandBanks = ArkhamInvestigator
  { arkhamInvestigatorName = "Roland Banks"
  , arkhamInvestigatorWillpower = 3
  , arkhamInvestigatorIntellect = 3
  , arkhamInvestigatorCombat = 4
  , arkhamInvestigatorAgility = 2
  , arkhamInvestigatorHealth = 9
  , arkhamInvestigatorSanity = 5
  , arkhamInvestigatorFrontImageUrl =
    "https://arkhamdb.com/bundles/cards/01001.png"
  , arkhamInvestigatorBackImageUrl =
    "https://arkhamdb.com/bundles/cards/01001b.png"
  }

study :: ArkhamLocation
study = ArkhamLocationRevealed $ ArkhamLocationRevealedData
  { arkhamLocationRevealedDataId = "1"
  , arkhamLocationRevealedDataName = "Study"
  , arkhamLocationRevealedDataSymbol = ArkhamLocationSymbolCircle
  , arkhamLocationRevealedDataConnections = []
  , arkhamLocationRevealedDataShroud = 2
  , arkhamLocationRevealedDataMaxClues = ArkhamClueCountPerInvestigator 2
  , arkhamLocationRevealedDataCurrentClues = 2
  , arkhamLocationRevealedDataImageUrl =
    "https://arkhamdb.com/bundles/cards/01111.png"
  }
