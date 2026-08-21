module Arkham.Story.CardDefs.ChildrenOfBlood where

import Arkham.Card.CardType
import Arkham.Story.CardDefs.Import

bloodToken :: CardDef
bloodToken =
  (emptyCardDef "13119" "Blood Token" StoryType)
    { cdDoubleSided = False
    , cdLevel = Nothing
    , cdMeta = mapFromList [("customBack", String "children_of_blood.avif")]
    }
