module Arkham.Treachery.CardDefs.TheDunwichLegacy.TheBeyond where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

arcaneBarrier :: CardDef
arcaneBarrier =
  (treachery "02102" "Arcane Barrier" TheBeyond 2)
    { cdCardTraits = setFromList [Hex, Obstacle]
    }

pushedIntoTheBeyond :: CardDef
pushedIntoTheBeyond =
  (treachery "02100" "Pushed into the Beyond" TheBeyond 2)
    { cdCardTraits = setFromList [Hex]
    }

terrorFromBeyond :: CardDef
terrorFromBeyond =
  (treachery "02101" "Terror from Beyond" TheBeyond 2)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril]
    }
