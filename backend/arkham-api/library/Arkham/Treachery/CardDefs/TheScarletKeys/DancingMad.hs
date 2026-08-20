module Arkham.Treachery.CardDefs.TheScarletKeys.DancingMad where

import Arkham.Treachery.CardDefs.Import

bodySnatched :: CardDef
bodySnatched =
  (treachery "09608" "Body Snatched" DancingMad 2)
    { cdCardTraits = setFromList [Scheme, Power]
    }
