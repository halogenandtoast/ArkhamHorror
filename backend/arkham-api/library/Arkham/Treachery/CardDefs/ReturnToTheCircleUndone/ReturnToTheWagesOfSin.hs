module Arkham.Treachery.CardDefs.ReturnToTheCircleUndone.ReturnToTheWagesOfSin where

import Arkham.Treachery.CardDefs.Import

witchweed :: CardDef
witchweed =
  peril
    (treachery "54040" "Witchweed" ReturnToTheWagesOfSin 2)
      { cdCardTraits = setFromList [Curse]
      }
