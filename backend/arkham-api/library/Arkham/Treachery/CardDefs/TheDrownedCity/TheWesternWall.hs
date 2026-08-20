module Arkham.Treachery.CardDefs.TheDrownedCity.TheWesternWall where

import Arkham.Treachery.CardDefs.Import

lookOut :: CardDef
lookOut =
  (treachery "11534" "\"Look Out!\"" TheWesternWall 3) {cdCardTraits = setFromList [Terror]}

seafloorFrieze :: CardDef
seafloorFrieze =
  (treachery "11531" "Seafloor Frieze" TheWesternWall 1)
    { cdCardTraits = setFromList [Evidence, Glyph]
    , cdOtherSide = Just "11531b"
    , cdDoubleSided = True
    , cdRevelation = CannotBeCanceledRevelation
    }
