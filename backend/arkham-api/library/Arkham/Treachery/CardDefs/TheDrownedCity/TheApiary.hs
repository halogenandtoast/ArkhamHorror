module Arkham.Treachery.CardDefs.TheDrownedCity.TheApiary where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

alienEggs :: CardDef
alienEggs =
  (treachery "11578" "Alien Eggs" TheApiary 3)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Surge]
    }

dangerousCuriosity :: CardDef
dangerousCuriosity =
  (treachery "11577" "Dangerous Curiosity" TheApiary 2) {cdCardTraits = setFromList [Blunder]}

hungryWalls :: CardDef
hungryWalls =
  (treachery "11576" "Hungry Walls" TheApiary 2) {cdCardTraits = setFromList [Hazard]}

parasiticTransformation :: CardDef
parasiticTransformation =
  (weakness "11583" "Parasitic Transformation")
    { cdCardTraits = setFromList [Curse, Hazard]
    , cdEncounterSet = Just TheApiary
    , cdEncounterSetQuantity = Just 4
    }
