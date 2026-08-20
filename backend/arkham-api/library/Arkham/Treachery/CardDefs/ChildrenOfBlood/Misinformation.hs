module Arkham.Treachery.CardDefs.ChildrenOfBlood.Misinformation where

import Arkham.Treachery.CardDefs.Import

misinformation :: CardDef
misinformation =
  (treachery "13108" "Misinformation" Misinformation 3)
    { cdCardTraits = singleton Scheme
    }
