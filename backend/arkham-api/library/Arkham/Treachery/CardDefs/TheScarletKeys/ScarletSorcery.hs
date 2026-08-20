module Arkham.Treachery.CardDefs.TheScarletKeys.ScarletSorcery where

import Arkham.Treachery.CardDefs.Import

boundInRed :: CardDef
boundInRed =
  (treachery "09729" "Bound in Red" ScarletSorcery 2)
    { cdCardTraits = setFromList [Hex]
    }

keyCharge :: CardDef
keyCharge =
  surge
    $ (treachery "09730" "Key Charge" ScarletSorcery 2)
      { cdCardTraits = setFromList [Hex]
      }
