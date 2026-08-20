module Arkham.Treachery.CardDefs.ThePathToCarcosa.TheStranger where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

markedByTheSign :: CardDef
markedByTheSign =
  (treachery "03104" "Marked by the Sign" TheStranger 2)
    { cdCardTraits = singleton Pact
    , cdKeywords = singleton Keyword.Peril
    }

thePaleMaskBeckons :: CardDef
thePaleMaskBeckons =
  (treachery "03105" "The Pale Mask Beckons" TheStranger 1)
    { cdCardTraits = setFromList [Omen, Pact]
    }
