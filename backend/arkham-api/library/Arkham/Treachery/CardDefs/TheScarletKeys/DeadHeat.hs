module Arkham.Treachery.CardDefs.TheScarletKeys.DeadHeat where

import Arkham.Treachery.CardDefs.Import

cornered :: CardDef
cornered =
  (treachery "09543" "Cornered!" DeadHeat 2)
    { cdCardTraits = setFromList [Hazard]
    }

famine :: CardDef
famine =
  (treachery "09542" "Famine" DeadHeat 2)
    { cdCardTraits = setFromList [Corruption]
    }
