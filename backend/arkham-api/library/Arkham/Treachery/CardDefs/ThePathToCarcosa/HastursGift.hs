module Arkham.Treachery.CardDefs.ThePathToCarcosa.HastursGift where

import Arkham.Treachery.CardDefs.Import

danceOfTheYellowKing :: CardDef
danceOfTheYellowKing =
  (treachery "03097" "Dance of the Yellow King" HastursGift 2)
    { cdCardTraits = singleton Pact
    }
