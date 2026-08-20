module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheLongestNight where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

endlessNight :: CardDef
endlessNight =
  (treachery "10649" "Endless Night" TheLongestNight 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

incursion :: CardDef
incursion =
  (treachery "10650" "Incursion" TheLongestNight 4)
    { cdCardTraits = singleton Scheme
    }
