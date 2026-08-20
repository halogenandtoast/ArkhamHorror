module Arkham.Treachery.CardDefs.TheDrownedCity.Rlyeh where

import Arkham.EncounterSet qualified as Set
import Arkham.Treachery.CardDefs.Import

crumblingMasonry :: CardDef
crumblingMasonry =
  (treachery "11740" "Crumbling Masonry" Set.Rlyeh 2) {cdCardTraits = setFromList [Hazard]}

cyclopeanArchitecture :: CardDef
cyclopeanArchitecture =
  (treachery "11739" "Cyclopean Architecture" Set.Rlyeh 2)
    { cdCardTraits = setFromList [Terror]
    }
