module Arkham.Treachery.CardDefs.TheScarletKeys.OnThinIce where

import Arkham.Treachery.CardDefs.Import

crackingIce :: CardDef
crackingIce =
  (treachery "09632" "Cracking Ice" OnThinIce 4)
    { cdCardTraits = setFromList [Hazard]
    }

snowslide :: CardDef
snowslide =
  (treachery "09633" "Snowslide" OnThinIce 2)
    { cdCardTraits = setFromList [Hazard]
    }
