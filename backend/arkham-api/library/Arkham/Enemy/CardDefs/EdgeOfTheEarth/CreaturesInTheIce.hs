module Arkham.Enemy.CardDefs.EdgeOfTheEarth.CreaturesInTheIce where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

glacialPhantasm :: CardDef
glacialPhantasm =
  (enemy "08690" "Glacial Phantasm" CreaturesInTheIce 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Eidolon]
    }

manifestationOfMadness :: CardDef
manifestationOfMadness =
  (enemy "08689" "Manifestation of Madness" CreaturesInTheIce 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
