module Arkham.Enemy.CardDefs.ThePathToCarcosa where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

graveyardGhouls :: CardDef
graveyardGhouls =
  (weakness "03017" "Graveyard Ghouls")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theThingThatFollows :: CardDef
theThingThatFollows =
  unique
    $ (basicWeakness "03042" "The Thing That Follows")
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Curse]
      , cdKeywords = setFromList [Keyword.Hunter]
      }
