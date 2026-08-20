module Arkham.Treachery.CardDefs.TheDunwichLegacy.LostInTimeAndSpace where

import Arkham.Treachery.CardDefs.Import

collapsingReality :: CardDef
collapsingReality =
  (treachery "02331" "Collapsing Reality" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Hazard]
    }

vastExpanse :: CardDef
vastExpanse =
  (treachery "02333" "Vast Expanse" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Terror]
    }

wormhole :: CardDef
wormhole =
  (treachery "02332" "Wormhole" LostInTimeAndSpace 2)
    { cdCardTraits = setFromList [Hazard]
    }
