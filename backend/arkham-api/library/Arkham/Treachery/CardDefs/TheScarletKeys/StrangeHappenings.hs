module Arkham.Treachery.CardDefs.TheScarletKeys.StrangeHappenings where

import Arkham.Treachery.CardDefs.Import

heavyRain :: CardDef
heavyRain =
  (treachery "09719" "Heavy Rain" StrangeHappenings 2)
    { cdCardTraits = setFromList [Hazard]
    }

pinchInReality :: CardDef
pinchInReality =
  (treachery "09718" "Pinch in Reality" StrangeHappenings 2)
    { cdCardTraits = setFromList [Power]
    }
