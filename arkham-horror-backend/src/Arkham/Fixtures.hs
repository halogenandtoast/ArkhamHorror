module Arkham.Fixtures where

import Arkham.Types
import Data.Map.Strict hiding (map)
import Data.Text (Text)
import Database.Persist
import Model
import Prelude (Int, const, flip, map, uncurry, ($))


rolandBanks :: ArkhamInvestigator
rolandBanks = ArkhamInvestigator
  { arkhamInvestigatorTitle = "Roland Banks"
  , arkhamInvestigatorSubtitle = "The Fed"
  , arkhamInvestigatorClass = "Guardian"
  , arkhamInvestigatorTraits = ["Agency", "Detective"]
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

nightOfTheZealot :: ArkhamProductSet
nightOfTheZealot = ArkhamProductSet
  { arkhamProductSetTitle = "Night of the Zealot"
  , arkhamProductSetType = "Cycle"
  }

nightOfTheZealotEncounters :: [(Text, Int)]
nightOfTheZealotEncounters =
  [ ("The Gathering", 16)
  , ("The Midnight Masks", 20)
  , ("The Devourer Below", 18)
  , ("Cult of Umôrdhoth", 5)
  , ("Ghouls", 7)
  , ("Rats", 3)
  , ("Chilling Cold", 4)
  , ("Striking Fear", 7)
  , ("Ancient Evils", 3)
  , ("Dark Cult", 6)
  , ("Nightgaunts", 4)
  , ("Locked Doors", 2)
  , ("Agents of Cthulhu", 4)
  , ("Agents of Hastur", 4)
  , ("Agents of Shub-Niggurath", 4)
  , ("Agents of Yog-Sothoth", 4)
  ]

allInvestigators :: [ArkhamInvestigator]
allInvestigators = [rolandBanks]

allProductSets :: [ArkhamProductSet]
allProductSets = [nightOfTheZealot]

encounterSets :: Map Text (ArkhamProductSetId -> [ArkhamEncounterSet])
encounterSets = fromList
  [ ( arkhamProductSetTitle nightOfTheZealot
    , \id ->
      map (uncurry (flip ArkhamEncounterSet id)) nightOfTheZealotEncounters
    )
  ]

encounterSetsFor :: Entity ArkhamProductSet -> [ArkhamEncounterSet]
encounterSetsFor (Entity id product) =
  findWithDefault (const []) (arkhamProductSetTitle product) encounterSets id

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
