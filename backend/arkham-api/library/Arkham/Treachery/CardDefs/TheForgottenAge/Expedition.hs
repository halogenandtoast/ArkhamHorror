module Arkham.Treachery.CardDefs.TheForgottenAge.Expedition where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

lostInTheWilds :: CardDef
lostInTheWilds =
  (treachery "04081" "Lost in the Wilds" Expedition 3)
    { cdCardTraits = singleton Blunder
    }

lowOnSupplies :: CardDef
lowOnSupplies =
  (treachery "04082" "Low on Supplies" Expedition 2)
    { cdCardTraits = singleton Blunder
    , cdKeywords = singleton Keyword.Peril
    }
