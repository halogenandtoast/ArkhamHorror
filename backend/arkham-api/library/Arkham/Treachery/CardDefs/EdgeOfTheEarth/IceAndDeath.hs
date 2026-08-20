module Arkham.Treachery.CardDefs.EdgeOfTheEarth.IceAndDeath where

import Arkham.Treachery.CardDefs.Import

apeirophobia :: CardDef
apeirophobia =
  (treachery "08516" "Apeirophobia" IceAndDeath 2)
    { cdCardTraits = setFromList [Terror]
    }

phantasmagoria :: CardDef
phantasmagoria =
  (treachery "08548" "Phantasmagoria" SeepingNightmares 2)
    { cdCardTraits = setFromList [Curse]
    }

zeroVisibility :: CardDef
zeroVisibility =
  (treachery "08517" "Zero Visibility" IceAndDeath 2)
    { cdCardTraits = setFromList [Hazard]
    }
