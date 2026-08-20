module Arkham.Treachery.CardDefs.TheForgottenAge.TheBoundaryBeyond where

import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

timelineDestabilization :: CardDef
timelineDestabilization =
  (treachery "04190" "Timeline Destabilization" EncounterSet.TheBoundaryBeyond 3)
    { cdCardTraits = singleton Hex
    }

windowToAnotherTime :: CardDef
windowToAnotherTime =
  (treachery "04189" "Window to Another Time" EncounterSet.TheBoundaryBeyond 3)
    { cdCardTraits = singleton Hex
    , cdKeywords = singleton Keyword.Peril
    }
