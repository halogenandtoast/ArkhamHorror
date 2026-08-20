module Arkham.Treachery.CardDefs.TheForgottenAge.CultOfPnakotus where

import Arkham.Treachery.CardDefs.Import

fromAnotherTime :: CardDef
fromAnotherTime =
  (treachery "53073" "From Another Time" CultOfPnakotus 2)
    { cdCardTraits = setFromList [Scheme]
    }
