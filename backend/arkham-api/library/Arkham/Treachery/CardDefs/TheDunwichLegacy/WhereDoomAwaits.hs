module Arkham.Treachery.CardDefs.TheDunwichLegacy.WhereDoomAwaits where

import Arkham.Treachery.CardDefs.Import

ritesHowled :: CardDef
ritesHowled =
  (treachery "02296" "Rites Howled" WhereDoomAwaits 3)
    { cdCardTraits = singleton Hex
    }

spacesBetween :: CardDef
spacesBetween =
  (treachery "02297" "Spaces Between" WhereDoomAwaits 3)
    { cdCardTraits = setFromList [Hex, Hazard]
    }

vortexOfTime :: CardDef
vortexOfTime =
  (treachery "02298" "Vortex of Time" WhereDoomAwaits 3)
    { cdCardTraits = setFromList [Hex, Hazard]
    }
