module Arkham.Treachery.CardDefs.TheScarletKeys.DealingsInTheDark where

import Arkham.Treachery.CardDefs.Import

accosted :: CardDef
accosted =
  (treachery "09588" "Accosted" DealingsInTheDark 2)
    { cdCardTraits = singleton Scheme
    }

lightOutOfVoid :: CardDef
lightOutOfVoid =
  peril
    $ (treachery "09589" "Light Out of Void" DealingsInTheDark 2)
      { cdCardTraits = singleton Hex
      }

shadowed :: CardDef
shadowed =
  (treachery "09587" "Shadowed" DealingsInTheDark 2)
    { cdCardTraits = singleton Scheme
    }
