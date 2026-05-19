module Arkham.Location.CardDefs.NightOfTheZealot where

import Arkham.Location.CardDefs.Import

study :: CardDef
study = location "01111" "Study" mempty Circle [] TheGathering

hallway :: CardDef
hallway =
  location
    "01112"
    "Hallway"
    mempty
    Square
    [Triangle, Plus, Diamond]
    TheGathering

attic :: CardDef
attic =
  victory 1 $ location "01113" "Attic" mempty Triangle [Square] TheGathering

cellar :: CardDef
cellar =
  victory 1 $ location "01114" "Cellar" mempty Plus [Square] TheGathering

parlor :: CardDef
parlor = location "01115" "Parlor" mempty Diamond [Square] TheGathering

yourHouse :: CardDef
yourHouse =
  location "01124" "Your House" [Arkham] Squiggle [Circle] TheMidnightMasks

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

downtownFirstBankOfArkham :: CardDef
downtownFirstBankOfArkham =
  location
    "01130"
    ("Downtown" <:> "First Bank of Arkham")
    [Arkham]
    Triangle
    [Moon, T]
    TheMidnightMasks

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

easttown :: CardDef
easttown =
  location "01132" "Easttown" [Arkham] Moon [Circle, Triangle] TheMidnightMasks

graveyard :: CardDef
graveyard =
  victory 1
    $ location "01133" "Graveyard" [Arkham] Hourglass [Circle] TheMidnightMasks

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

mainPath :: CardDef
mainPath =
  location "01149" "Main Path" [Woods] Squiggle [Square, Plus] TheDevourerBelow

arkhamWoodsUnhallowedGround :: CardDef
arkhamWoodsUnhallowedGround =
  locationWithUnrevealed
    "01150"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Unhallowed Ground")
    [Woods]
    Triangle
    [Squiggle, Hourglass, Diamond]
    TheDevourerBelow

arkhamWoodsTwistingPaths :: CardDef
arkhamWoodsTwistingPaths =
  locationWithUnrevealed
    "01151"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Twisting Paths")
    [Woods]
    T
    [Squiggle, Diamond, Equals]
    TheDevourerBelow

arkhamWoodsOldHouse :: CardDef
arkhamWoodsOldHouse =
  locationWithUnrevealed
    "01152"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Old House")
    [Woods]
    Diamond
    [Squiggle, Triangle, T]
    TheDevourerBelow

arkhamWoodsCliffside :: CardDef
arkhamWoodsCliffside =
  locationWithUnrevealed
    "01153"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Cliffside")
    [Woods]
    Hourglass
    [Squiggle, Moon, Triangle]
    TheDevourerBelow

arkhamWoodsTangledThicket :: CardDef
arkhamWoodsTangledThicket =
  locationWithUnrevealed
    "01154"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Tangled Thicket")
    [Woods]
    Equals
    [Squiggle, T, Moon]
    TheDevourerBelow

arkhamWoodsQuietGlade :: CardDef
arkhamWoodsQuietGlade =
  locationWithUnrevealed
    "01155"
    "Arkham Woods"
    [Woods]
    Square
    [Squiggle]
    ("Arkham Woods" <:> "Quiet Glade")
    [Woods]
    Moon
    [Squiggle, Equals, Hourglass]
    TheDevourerBelow

ritualSite :: CardDef
ritualSite =
  location "01156" "Ritual Site" [Cave] Plus [Squiggle] TheDevourerBelow
