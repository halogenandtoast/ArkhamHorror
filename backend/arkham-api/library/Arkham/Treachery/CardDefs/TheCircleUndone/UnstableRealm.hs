module Arkham.Treachery.CardDefs.TheCircleUndone.UnstableRealm where

import Arkham.Treachery.CardDefs.Import

fromTheOtherSide :: CardDef
fromTheOtherSide =
  (treachery "54069" "From the Other Side" UnstableRealm 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }

unstableEnergies :: CardDef
unstableEnergies =
  (treachery "54068" "Unstable Energies" UnstableRealm 2)
    { cdCardTraits = setFromList [Hazard, Spectral]
    }
