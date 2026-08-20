module Arkham.Treachery.CardDefs.EdgeOfTheEarth.DeadlyWeather where

import Arkham.Treachery.CardDefs.Import

antarcticWind :: CardDef
antarcticWind =
  (treachery "08692" "Antarctic Wind" DeadlyWeather 2)
    { cdCardTraits = setFromList [Hazard]
    }

polarVortex :: CardDef
polarVortex =
  (treachery "08694" "Polar Vortex" DeadlyWeather 2)
    { cdCardTraits = setFromList [Hazard]
    }

whiteout :: CardDef
whiteout =
  (treachery "08693" "Whiteout" DeadlyWeather 2)
    { cdCardTraits = setFromList [Hazard]
    }
