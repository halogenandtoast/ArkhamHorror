module Arkham.Treachery.CardDefs.NightOfTheZealot.GhoulsOfUmordhoth where

import Arkham.Treachery.CardDefs.Import

chillFromBelow :: CardDef
chillFromBelow =
  (treachery "50040" "Chill from Below" GhoulsOfUmordhoth 3)
    { cdCardTraits = setFromList [Hazard]
    }
