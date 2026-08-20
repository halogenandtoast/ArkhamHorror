module Arkham.Treachery.CardDefs.ThePathToCarcosa.DecayingReality where

import Arkham.Treachery.CardDefs.Import

bleedingWalls :: CardDef
bleedingWalls =
  (treachery "52066" "Bleeding Walls" DecayingReality 2)
    { cdCardTraits = setFromList [Terror]
    }

fragileThoughts :: CardDef
fragileThoughts =
  (treachery "52067" "Fragile Thoughts" DecayingReality 2)
    { cdCardTraits = setFromList [Terror]
    }
