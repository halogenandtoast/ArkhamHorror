module Arkham.Enemy.CardDefs.Promo where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

sacrificialBeast :: CardDef
sacrificialBeast =
  (weakness "98003" "Sacrificial Beast")
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, DarkYoung]
    , cdKeywords = singleton Keyword.Replacement
    }

vengefulHound :: CardDef
vengefulHound =
  (weakness "98009" "Vengeful Hound")
    { cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = singleton Keyword.Replacement
    }
