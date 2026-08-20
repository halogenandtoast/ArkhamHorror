module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.CreepingCold where

import Arkham.Treachery.CardDefs.Import

inexplicableCold :: CardDef
inexplicableCold =
  (treachery "51066" "Inexplicable Cold" CreepingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }

oppressiveMists :: CardDef
oppressiveMists =
  (treachery "51067" "Oppressive Mists" CreepingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }
