module Arkham.Treachery.CardDefs.EdgeOfTheEarth.HazardsOfAntarctica where

import Arkham.Treachery.CardDefs.Import

iceShaft :: CardDef
iceShaft =
  (treachery "08698" "Ice Shaft" HazardsOfAntarctica 3)
    { cdCardTraits = setFromList [Hazard]
    }

throughTheIce :: CardDef
throughTheIce =
  (treachery "08699" "Through the Ice" HazardsOfAntarctica 2)
    { cdCardTraits = setFromList [Hazard]
    }
