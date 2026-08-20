module Arkham.Treachery.CardDefs.ThePathToCarcosa.Hauntings where

import Arkham.Treachery.CardDefs.Import

spiritsTorment :: CardDef
spiritsTorment =
  (treachery "03094" "Spirit's Torment" Hauntings 2)
    { cdCardTraits = setFromList [Curse, Geist]
    }
