module Arkham.Treachery.CardDefs.TheScarletKeys.BeyondTheBeyond where

import Arkham.Treachery.CardDefs.Import

paradimensionalTerror :: CardDef
paradimensionalTerror =
  peril
    $ (treachery "09751" "Paradimensional Terror" BeyondTheBeyond 2)
      { cdCardTraits = setFromList [Terror]
      }
