module Arkham.Story.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Prelude
import Arkham.Trait

addTrait :: Trait -> CardDef -> CardDef
addTrait trait def =
  def
    { cdCardTraits = insertSet trait (cdCardTraits def)
    }

doubleSided :: CardDef -> CardDef
doubleSided def =
  def
    { cdDoubleSided = True
    , cdOtherSide = Just $ flippedCardCode def.cardCode
    }

otherSideIs :: CardCode -> CardDef -> CardDef
otherSideIs ccode def =
  def
    { cdDoubleSided = True
    , cdOtherSide = Just ccode
    }

story :: CardCode -> Name -> EncounterSet -> CardDef
story cardCode name encounterSet =
  (emptyCardDef cardCode name StoryType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = False
    , cdLevel = Nothing
    }

victory :: Int -> CardDef -> CardDef
victory n def = def {cdVictoryPoints = Just n}

cthulhuDeckBack :: Map Text Value
cthulhuDeckBack = mapFromList [("customBack", String "back_cthulhu_deck.jpg")]

-- | The quantity is the number of copies in the 18-card Cthulhu deck.
cthulhuDeckCard :: CardCode -> Name -> Int -> EncounterSet -> CardDef
cthulhuDeckCard cCode name quantity encounterSet =
  (story cCode name encounterSet) {cdMeta = cthulhuDeckBack, cdEncounterSetQuantity = Just quantity}
