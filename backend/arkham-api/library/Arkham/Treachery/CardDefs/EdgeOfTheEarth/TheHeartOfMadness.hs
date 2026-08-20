module Arkham.Treachery.CardDefs.EdgeOfTheEarth.TheHeartOfMadness where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

electrostaticDischarge :: CardDef
electrostaticDischarge =
  (treachery "08670" "Electrostatic Discharge" TheGreatSeal 2)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Surge]
    }

primevalTerror :: CardDef
primevalTerror =
  (treachery "08657" "Primeval Terror" TheHeartOfMadness 3)
    { cdCardTraits = setFromList [Terror]
    }

rootsOfTheEarth :: CardDef
rootsOfTheEarth =
  (treachery "08658" "Roots of the Earth" TheHeartOfMadness 3)
    { cdCardTraits = setFromList [Hazard]
    }
