module Arkham.Treachery.CardDefs.ThePathToCarcosa.TheLastKing where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

fineDining :: CardDef
fineDining =
  (treachery "03082" "Fine Dining" TheLastKing 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

toughCrowd :: CardDef
toughCrowd =
  (treachery "03083" "Tough Crowd" TheLastKing 2)
    { cdCardTraits = singleton Hazard
    }
