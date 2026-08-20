module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheForest where

import Arkham.Treachery.CardDefs.Import

bloom :: CardDef
bloom =
  (treachery "10735" "Bloom" TheForest 2)
    { cdCardTraits = setFromList [Power]
    }

callOfTheWild :: CardDef
callOfTheWild =
  (treachery "10737" "Call of the Wild" TheForest 2)
    { cdCardTraits = setFromList [Terror]
    }

wallOfThorns :: CardDef
wallOfThorns =
  (treachery "10736" "Wall of Thorns" TheForest 2)
    { cdCardTraits = setFromList [Hazard, Flora]
    }
