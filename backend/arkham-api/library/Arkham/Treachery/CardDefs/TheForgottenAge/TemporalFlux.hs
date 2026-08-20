module Arkham.Treachery.CardDefs.TheForgottenAge.TemporalFlux where

import Arkham.Treachery.CardDefs.Import

aTearInTime :: CardDef
aTearInTime =
  (treachery "04090" "A Tear in Time" TemporalFlux 3)
    { cdCardTraits = singleton Hex
    }

lostInTime :: CardDef
lostInTime =
  (treachery "04091" "Lost in Time" TemporalFlux 2)
    { cdCardTraits = singleton Hex
    }
