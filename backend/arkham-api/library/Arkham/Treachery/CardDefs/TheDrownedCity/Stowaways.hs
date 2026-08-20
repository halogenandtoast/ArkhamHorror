module Arkham.Treachery.CardDefs.TheDrownedCity.Stowaways where

import Arkham.Treachery.CardDefs.Import

infected :: CardDef
infected =
  (treachery "11722" "Infected!" Stowaways 3) {cdCardTraits = setFromList [Hazard]}
