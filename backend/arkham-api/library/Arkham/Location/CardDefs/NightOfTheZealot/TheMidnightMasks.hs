module Arkham.Location.CardDefs.NightOfTheZealot.TheMidnightMasks where

import Arkham.Location.CardDefs.Import

downtownArkhamAsylum :: CardDef
downtownArkhamAsylum =
  victory 1
    $ location
      "01131"
      ("Downtown" <:> "Arkham Asylum")
      [Arkham]
      Triangle
      [Moon, T]
      TheMidnightMasks

downtownFirstBankOfArkham :: CardDef
downtownFirstBankOfArkham =
  location
    "01130"
    ("Downtown" <:> "First Bank of Arkham")
    [Arkham]
    Triangle
    [Moon, T]
    TheMidnightMasks

easttown :: CardDef
easttown =
  location "01132" "Easttown" [Arkham] Moon [Circle, Triangle] TheMidnightMasks

graveyard :: CardDef
graveyard =
  victory 1
    $ location "01133" "Graveyard" [Arkham] Hourglass [Circle] TheMidnightMasks

miskatonicUniversity :: CardDef
miskatonicUniversity =
  victory 1
    $ location
      "01129"
      "Miskatonic University"
      [Arkham]
      Diamond
      [T, Plus, Circle, Square]
      TheMidnightMasks

northside :: CardDef
northside =
  victory 1
    $ location
      "01134"
      "Northside"
      [Arkham]
      T
      [Diamond, Triangle]
      TheMidnightMasks

rivertown :: CardDef
rivertown =
  location
    "01125"
    "Rivertown"
    [Arkham, Central]
    Circle
    [Moon, Diamond, Square, Squiggle, Hourglass]
    TheMidnightMasks

southsideHistoricalSociety :: CardDef
southsideHistoricalSociety =
  location
    "01126"
    ("Southside" <:> "Historical Society")
    [Arkham]
    Square
    [Diamond, Plus, Circle]
    TheMidnightMasks

southsideMasBoardingHouse :: CardDef
southsideMasBoardingHouse =
  location
    "01127"
    ("Southside" <:> "Ma's Boarding House")
    [Arkham]
    Square
    [Diamond, Plus, Circle]
    TheMidnightMasks

stMarysHospital :: CardDef
stMarysHospital =
  location
    "01128"
    "St. Mary's Hospital"
    [Arkham]
    Plus
    [Diamond, Square]
    TheMidnightMasks

yourHouse :: CardDef
yourHouse =
  location "01124" "Your House" [Arkham] Squiggle [Circle] TheMidnightMasks
