module Arkham.Treachery.CardDefs.TheScarletKeys.SpatialAnomaly where

import Arkham.Treachery.CardDefs.Import

beyondThePale :: CardDef
beyondThePale =
  (treachery "09742" "Beyond the Pale" SpatialAnomaly 3)
    { cdCardTraits = setFromList [Hex]
    }

splinteredSpace :: CardDef
splinteredSpace =
  (treachery "09741" "Splintered Space" SpatialAnomaly 3)
    { cdCardTraits = setFromList [Hex]
    }

warpedReality :: CardDef
warpedReality =
  (treachery "09743" "Warped Reality" SpatialAnomaly 2)
    { cdCardTraits = setFromList [Hex]
    }
