module Arkham.Enemy.CardDefs.ThePathToCarcosa.HastursEnvoys where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

preyingByakhee :: CardDef
preyingByakhee =
  (enemy "52069" "Preying Byakhee" HastursEnvoys 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
