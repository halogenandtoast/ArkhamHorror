module Arkham.Treachery.CardDefs.TheCircleUndone.SpectralPredators where

import Arkham.Treachery.CardDefs.Import

whispersInTheDark :: CardDef
whispersInTheDark =
  (treachery "05102" "Whispers in the Dark" SpectralPredators 2)
    { cdCardTraits = setFromList [Omen, Spectral]
    }
