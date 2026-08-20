module Arkham.Treachery.CardDefs.NightOfTheZealot.TheMidnightMasks where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

falseLead :: CardDef
falseLead = treachery "01136" "False Lead" TheMidnightMasks 2

huntingShadow :: CardDef
huntingShadow =
  (treachery "01135" "Hunting Shadow" TheMidnightMasks 3)
    { cdCardTraits = setFromList [Curse]
    , cdKeywords = setFromList [Keyword.Peril]
    }
