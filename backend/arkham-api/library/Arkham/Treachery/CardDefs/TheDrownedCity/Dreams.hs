module Arkham.Treachery.CardDefs.TheDrownedCity.Dreams where

import Arkham.Treachery.CardDefs.Import

drawnToDarkness :: CardDef
drawnToDarkness =
  (treachery "11750" "Drawn to Darkness" Dreams 3) {cdCardTraits = setFromList [Power]}

torturedVisions :: CardDef
torturedVisions =
  (treachery "11749" "Tortured Visions" Dreams 3) {cdCardTraits = setFromList [Terror]}
