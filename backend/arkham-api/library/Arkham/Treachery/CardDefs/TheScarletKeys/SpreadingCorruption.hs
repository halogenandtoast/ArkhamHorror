module Arkham.Treachery.CardDefs.TheScarletKeys.SpreadingCorruption where

import Arkham.Treachery.CardDefs.Import

compulsion :: CardDef
compulsion =
  (treachery "09745" "Compulsion" SpreadingCorruption 2)
    { cdCardTraits = setFromList [Curse, Terror]
    }

distortedReasoning :: CardDef
distortedReasoning =
  (treachery "09746" "Distorted Reasoning" SpreadingCorruption 2)
    { cdCardTraits = setFromList [Curse, Terror]
    }

touchOfTheBeyond :: CardDef
touchOfTheBeyond =
  (treachery "09744" "Touch of the Beyond" SpreadingCorruption 2)
    { cdCardTraits = setFromList [Curse]
    }
