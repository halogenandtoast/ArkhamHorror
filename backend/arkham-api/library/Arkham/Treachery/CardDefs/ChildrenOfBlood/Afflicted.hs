module Arkham.Treachery.CardDefs.ChildrenOfBlood.Afflicted where

import Arkham.Treachery.CardDefs.Import

frenziedHunger :: CardDef
frenziedHunger =
  (treachery "13094" "Frenzied Hunger" Afflicted 2)
    { cdCardTraits = singleton Curse
    }

torturousTransformation :: CardDef
torturousTransformation =
  (treachery "13095" "Torturous Transformation" Afflicted 3)
    { cdCardTraits = setFromList [Power, Blight]
    }

rottingRemains :: CardDef
rottingRemains =
  (treachery "13096" "Rotting Remains" Afflicted 2)
    { cdCardTraits = singleton Terror
    }
