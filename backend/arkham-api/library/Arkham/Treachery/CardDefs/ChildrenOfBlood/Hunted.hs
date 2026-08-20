module Arkham.Treachery.CardDefs.ChildrenOfBlood.Hunted where

import Arkham.Treachery.CardDefs.Import

ofTheNight :: CardDef
ofTheNight =
  (treachery "13106" "Of the Night" Hunted 2)
    { cdCardTraits = singleton Terror
    }

nowhereToHide :: CardDef
nowhereToHide =
  (treachery "13107" "Nowhere to Hide" Hunted 2)
    { cdCardTraits = singleton Terror
    }
