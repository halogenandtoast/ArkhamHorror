module Arkham.Location.CardDefs.ReturnToNightOfTheZealot.ReturnToTheMidnightMasks where

import Arkham.Location.CardDefs.Import

easttownArkhamPoliceStation :: CardDef
easttownArkhamPoliceStation =
  victory 1
    $ locationWithUnrevealed
      "50027"
      "Easttown"
      [Arkham]
      Moon
      [Circle, Triangle]
      ("Easttown" <:> "Arkham Police Station")
      [Arkham]
      Moon
      [Circle, Triangle]
      ReturnToTheMidnightMasks

miskatonicUniversityMiskatonicMuseum :: CardDef
miskatonicUniversityMiskatonicMuseum =
  locationWithUnrevealed
    "50029"
    "Miskatonic University"
    [Arkham]
    Diamond
    [T, Plus, Circle, Square]
    ("Miskatonic University" <:> "Miskatonic Museum")
    [Arkham]
    Diamond
    [T, Plus, Circle, Square]
    ReturnToTheMidnightMasks

northsideTrainStation :: CardDef
northsideTrainStation =
  locationWithUnrevealed
    "50028"
    ("Northside" <:> "Train Station")
    [Arkham]
    T
    [Diamond, Triangle]
    ("Northside" <:> "Train Station")
    [Arkham]
    T
    [Diamond, Triangle]
    ReturnToTheMidnightMasks

rivertownAbandonedWarehouse :: CardDef
rivertownAbandonedWarehouse =
  locationWithUnrevealed
    "50030"
    "Rivertown"
    [Arkham, Central]
    Circle
    [Moon, Diamond, Square, Squiggle, Hourglass]
    ("Rivertown" <:> "Abandoned Warehouse")
    [Arkham, Central]
    Circle
    [Moon, Diamond, Square, Squiggle, Hourglass]
    ReturnToTheMidnightMasks
