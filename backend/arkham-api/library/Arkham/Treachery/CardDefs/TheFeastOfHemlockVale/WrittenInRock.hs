module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.WrittenInRock where

import Arkham.Treachery.CardDefs.Import

caveIn :: CardDef
caveIn =
  (treachery "10520" "Cave-In" WrittenInRock 2)
    { cdCardTraits = setFromList [Hazard]
    }

wildRide :: CardDef
wildRide =
  (treachery "10521" "Wild Ride" WrittenInRock 3)
    { cdCardTraits = setFromList [Hazard]
    }
