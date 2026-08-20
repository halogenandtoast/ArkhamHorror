module Arkham.Treachery.CardDefs.EdgeOfTheEarth.AgentsOfTheUnknown where

import Arkham.Treachery.CardDefs.Import

theMadnessWithin :: CardDef
theMadnessWithin =
  (treachery "08688" "The Madness Within" AgentsOfTheUnknown 2)
    { cdCardTraits = setFromList [Curse]
    }
