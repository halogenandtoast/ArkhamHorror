module Arkham.Treachery.CardDefs.NightOfTheZealot.TheDevourersCult where

import Arkham.Treachery.CardDefs.Import

maskOfUmordhoth :: CardDef
maskOfUmordhoth =
  (treachery "50043" "Mask of Umôrdhoth" TheDevourersCult 2)
    { cdCardTraits = setFromList [Item, Mask]
    }
