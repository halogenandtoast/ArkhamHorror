module Arkham.Treachery.CardDefs.TheDrownedCity.UnderseaCreatures where

import Arkham.Treachery.CardDefs.Import

dreamingMigration :: CardDef
dreamingMigration =
  (treachery "11734" "Dreaming Migration" UnderseaCreatures 2)
    { cdCardTraits = setFromList [Hazard]
    }

underseaHunt :: CardDef
underseaHunt =
  (treachery "11735" "Undersea Hunt" UnderseaCreatures 2) {cdCardTraits = setFromList [Scheme]}
