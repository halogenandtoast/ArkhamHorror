module Arkham.Agenda.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Prelude hiding (fold)

agenda :: CardCode -> Name -> Int -> EncounterSet -> CardDef
agenda cardCode name stage encounterSet =
  (emptyCardDef cardCode name AgendaType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Nothing
    , cdDoubleSided = True
    , cdStage = Just stage
    , cdLevel = Nothing
    }

otherSideIs :: CardCode -> CardDef -> CardDef
otherSideIs ccode def =
  def
    { cdDoubleSided = False
    , cdOtherSide = Just ccode
    }
