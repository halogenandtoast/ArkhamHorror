module Arkham.Enemy.CardDefs.Promo where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

sacrificialBeast :: CardDef
sacrificialBeast =
  (weakness "98003" "Sacrificial Beast")
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdKeywords = singleton Keyword.Replacement
    }

vengefulHound :: CardDef
vengefulHound =
  (weakness "98009" "Vengeful Hound")
    { cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = singleton Keyword.Replacement
    }
