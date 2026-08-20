module Arkham.Treachery.CardDefs.TheCircleUndone.ChillingMists where

import Arkham.Treachery.CardDefs.Import

mistsFromBeyond :: CardDef
mistsFromBeyond =
  (treachery "54073" "Mists from Beyond" ChillingMists 2)
    { cdCardTraits = setFromList [Hazard]
    }

supernaturalTempest :: CardDef
supernaturalTempest =
  (treachery "54072" "Supernatural Tempest" ChillingMists 2)
    { cdCardTraits = setFromList [Hazard]
    }
