module Arkham.Treachery.CardDefs.TheCircleUndone.TrappedSpirits where

import Arkham.Treachery.CardDefs.Import

trappedSpirits :: CardDef
trappedSpirits =
  (treachery "05104" "Trapped Spirits" TrappedSpirits 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }
