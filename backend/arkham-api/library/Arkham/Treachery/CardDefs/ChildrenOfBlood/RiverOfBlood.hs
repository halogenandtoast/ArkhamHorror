module Arkham.Treachery.CardDefs.ChildrenOfBlood.RiverOfBlood where

import Arkham.Treachery.CardDefs.Import

burningDaylight :: CardDef
burningDaylight =
  (treachery "13028" "Burning Daylight" RiverOfBlood 3)
    { cdCardTraits = singleton Scheme
    }
