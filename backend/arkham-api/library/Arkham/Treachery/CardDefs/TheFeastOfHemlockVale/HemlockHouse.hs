module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.HemlockHouse where

import Arkham.Treachery.CardDefs.Import

outOfTheWalls :: CardDef
outOfTheWalls =
  (treachery "10545" "Out of the Walls" HemlockHouse 4)
    { cdCardTraits = setFromList [Hazard]
    }

pulledIn :: CardDef
pulledIn =
  (treachery "10546" "Pulled In" HemlockHouse 2)
    { cdCardTraits = setFromList [Blunder]
    }
