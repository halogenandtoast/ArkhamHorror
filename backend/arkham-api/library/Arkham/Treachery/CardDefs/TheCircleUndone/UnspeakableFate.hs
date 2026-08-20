module Arkham.Treachery.CardDefs.TheCircleUndone.UnspeakableFate where

import Arkham.Treachery.CardDefs.Import

fateOfAllFools :: CardDef
fateOfAllFools =
  (treachery "54067" "Fate of All Fools" UnspeakableFate 3)
    { cdCardTraits = setFromList [Omen, Spectral]
    }

unavoidableDemise :: CardDef
unavoidableDemise =
  (treachery "54066" "Unavoidable Demise" UnspeakableFate 3)
    { cdCardTraits = setFromList [Hazard, Spectral]
    }
