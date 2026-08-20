module Arkham.Treachery.CardDefs.EdgeOfTheEarth.Penguins where

import Arkham.Treachery.CardDefs.Import

wukWukWuk :: CardDef
wukWukWuk =
  (treachery "08709" "Wuk! Wuk! Wuk!" Penguins 2)
    { cdCardTraits = setFromList [Curse, Hazard]
    }
