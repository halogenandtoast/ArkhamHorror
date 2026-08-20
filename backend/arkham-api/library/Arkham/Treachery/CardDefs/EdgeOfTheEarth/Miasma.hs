module Arkham.Treachery.CardDefs.EdgeOfTheEarth.Miasma where

import Arkham.Treachery.CardDefs.Import

miasmaticTorment :: CardDef
miasmaticTorment =
  (treachery "08706" "Miasmatic Torment" Miasma 2)
    { cdCardTraits = setFromList [Curse]
    }

nebulousMiasma :: CardDef
nebulousMiasma =
  (treachery "08707" "Nebulous Miasma" Miasma 2)
    { cdCardTraits = setFromList [Curse, Hazard]
    }
