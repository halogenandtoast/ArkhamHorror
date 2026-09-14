module Arkham.Treachery.CardDefs.NightOfTheZealot.TheDevourerBelow where

import Arkham.Treachery.CardDefs.Import

umordhothsWrath :: CardDef
umordhothsWrath =
  (treachery "01158" "Umôrdhoth's Wrath" TheDevourerBelow 2)
    { cdArtVariants = mapFromList [("revised", "01658")]
    , cdCardTraits = setFromList [Curse]
    }
