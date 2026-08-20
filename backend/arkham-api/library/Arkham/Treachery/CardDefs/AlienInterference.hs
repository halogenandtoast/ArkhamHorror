module Arkham.Treachery.CardDefs.AlienInterference where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

encephalonSignal :: CardDef
encephalonSignal =
  (treachery "84030" "Encephalon Signal" AlienInterference 3)
    { cdCardTraits = singleton Hazard
    , cdKeywords = setFromList [Keyword.Peril]
    }
