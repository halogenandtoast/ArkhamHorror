module Arkham.Treachery.CardDefs.EdgeOfTheEarth.ToTheForbiddenPeaks where

import Arkham.Treachery.CardDefs.Import

avalanche :: CardDef
avalanche =
  (treachery "08611" "Avalance" ToTheForbiddenPeaks 2)
    { cdCardTraits = setFromList [Hazard]
    }

hangingOnTheEdge :: CardDef
hangingOnTheEdge =
  (treachery "08612" "Hanging on the Edge" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Hazard]
    }

hypothermia :: CardDef
hypothermia =
  (treachery "08613" "Hypothermia" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Hazard]
    }

snowfall :: CardDef
snowfall =
  (treachery "08610" "Snowfall" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Hazard]
    }
