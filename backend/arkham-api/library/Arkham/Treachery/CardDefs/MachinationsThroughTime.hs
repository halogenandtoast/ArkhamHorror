module Arkham.Treachery.CardDefs.MachinationsThroughTime where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

aTearInTime :: CardDef
aTearInTime =
  (treachery "87048" "A Tear in Time" MachinationsThroughTime 2)
    { cdCardTraits = singleton Hex
    }

abducted :: CardDef
abducted =
  (treachery "87049" "Abducted" MachinationsThroughTime 3)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Peril
    }

brokenSpace :: CardDef
brokenSpace =
  (treachery "87050" "Broken Space" MachinationsThroughTime 3)
    { cdCardTraits = singleton Hex
    }

dimensionalBreach :: CardDef
dimensionalBreach =
  (treachery "87051" "Dimensional Breach" MachinationsThroughTime 2)
    { cdCardTraits = setFromList [Hazard, Power]
    }

fromAllAngles :: CardDef
fromAllAngles =
  (treachery "87052" "From All Angles" MachinationsThroughTime 3)
    { cdCardTraits = singleton Terror
    }

lostInTime :: CardDef
lostInTime =
  (treachery "87053" "Lost in Time" MachinationsThroughTime 1)
    { cdCardTraits = singleton Hex
    }

mergingTimelines :: CardDef
mergingTimelines =
  (treachery "87054" "Merging Timelines" MachinationsThroughTime 2)
    { cdCardTraits = singleton Hex
    }

openPortal :: CardDef
openPortal =
  (treachery "87055" "Open Portal" MachinationsThroughTime 3)
    { cdCardTraits = singleton Hazard
    }

temporalDistortion :: CardDef
temporalDistortion =
  (treachery "87056" "Temporal Distortion" MachinationsThroughTime 2)
    { cdCardTraits = singleton Hazard
    }

vanishingHistory :: CardDef
vanishingHistory =
  (treachery "87057" "Vanishing History" MachinationsThroughTime 3)
    { cdCardTraits = singleton Hex
    }
