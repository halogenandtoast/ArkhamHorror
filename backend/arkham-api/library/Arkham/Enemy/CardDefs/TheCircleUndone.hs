{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheCircleUndone where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

hoods :: CardDef
hoods =
  (weakness "05017" "Hoods")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }
