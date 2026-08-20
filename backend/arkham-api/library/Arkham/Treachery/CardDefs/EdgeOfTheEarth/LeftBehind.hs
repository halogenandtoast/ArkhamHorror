module Arkham.Treachery.CardDefs.EdgeOfTheEarth.LeftBehind where

import Arkham.Treachery.CardDefs.Import

abandonedToMadness :: CardDef
abandonedToMadness =
  (treachery "08702" "Abandoned to Madness" LeftBehind 2)
    { cdCardTraits = setFromList [Curse]
    }
