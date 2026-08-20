module Arkham.Location.CardDefs.ReturnToTheDunwichLegacy.ReturnToExtracurricularActivities where

import Arkham.Location.CardDefs.Import

warrenObservatory :: CardDef
warrenObservatory =
  victory 1
    $ location
      "51013"
      "Warren Observatory"
      [Miskatonic]
      Triangle
      [Plus, Square]
      ReturnToExtracurricularActivities
