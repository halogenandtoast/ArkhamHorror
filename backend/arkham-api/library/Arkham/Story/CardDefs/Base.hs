module Arkham.Story.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.LocationSymbol qualified as LS
import Arkham.Name
import Arkham.Trait
import Arkham.Prelude

withScanIcons :: [LS.LocationSymbol] -> CardDef -> CardDef
withScanIcons icons def = def {cdMeta = insertMap "scanIcons" (toJSON icons) def.meta}

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
