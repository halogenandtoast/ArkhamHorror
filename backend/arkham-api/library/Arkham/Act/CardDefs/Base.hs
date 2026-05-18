module Arkham.Act.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Prelude hiding (fold)

act :: CardCode -> Name -> Int -> EncounterSet -> CardDef
act cardCode name stage encounterSet =
  (emptyCardDef cardCode name ActType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Nothing
    , cdDoubleSided = True
    , cdStage = Just stage
    , cdLevel = Nothing
    }
