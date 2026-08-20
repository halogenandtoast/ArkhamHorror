module Arkham.Treachery.CardDefs.TheDrownedCity.AlienMachinery where

import Arkham.Treachery.CardDefs.Import

infernalMachinery :: CardDef
infernalMachinery =
  (treachery "11752" "Infernal Machinery" AlienMachinery 2)
    { cdCardTraits = setFromList [Hazard]
    }
