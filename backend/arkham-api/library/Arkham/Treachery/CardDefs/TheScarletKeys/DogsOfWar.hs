module Arkham.Treachery.CardDefs.TheScarletKeys.DogsOfWar where

import Arkham.Treachery.CardDefs.Import

locusPulse :: CardDef
locusPulse =
  (treachery "09658" "Locus Pulse" DogsOfWar 2)
    { cdCardTraits = setFromList [Hex]
    }
