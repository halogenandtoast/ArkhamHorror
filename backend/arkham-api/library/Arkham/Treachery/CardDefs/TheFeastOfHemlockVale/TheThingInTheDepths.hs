module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheThingInTheDepths where

import Arkham.Treachery.CardDefs.Import

groundDisturbance :: CardDef
groundDisturbance =
  (treachery "10603" "Ground Disturbance" TheThingInTheDepths 3)
    { cdCardTraits = setFromList [Hazard]
    }

sinkingSludge :: CardDef
sinkingSludge =
  (treachery "10604" "Sinking Sludge" TheThingInTheDepths 4)
    { cdCardTraits = singleton Hazard
    }
