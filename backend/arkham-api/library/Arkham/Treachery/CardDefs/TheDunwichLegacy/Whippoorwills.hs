module Arkham.Treachery.CardDefs.TheDunwichLegacy.Whippoorwills where

import Arkham.Treachery.CardDefs.Import

eagerForDeath :: CardDef
eagerForDeath =
  (treachery "02091" "Eager for Death" Whippoorwills 2)
    { cdCardTraits = setFromList [Omen]
    }
