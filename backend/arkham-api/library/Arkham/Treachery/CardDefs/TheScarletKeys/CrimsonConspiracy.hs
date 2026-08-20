module Arkham.Treachery.CardDefs.TheScarletKeys.CrimsonConspiracy where

import Arkham.Treachery.CardDefs.Import

conspiracyInRed :: CardDef
conspiracyInRed =
  (treachery "09717" "Conspiracy in Red" CrimsonConspiracy 2)
    { cdCardTraits = setFromList [Scheme]
    }
