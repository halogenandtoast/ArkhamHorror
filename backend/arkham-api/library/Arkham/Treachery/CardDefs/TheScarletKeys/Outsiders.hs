module Arkham.Treachery.CardDefs.TheScarletKeys.Outsiders where

import Arkham.Treachery.CardDefs.Import

substanceDissimulation :: CardDef
substanceDissimulation =
  (treachery "09733" "Substance Dissimulation" Outsiders 2)
    { cdCardTraits = setFromList [Power]
    }
