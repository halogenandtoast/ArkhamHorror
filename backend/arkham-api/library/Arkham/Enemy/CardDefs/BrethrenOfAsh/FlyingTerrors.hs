module Arkham.Enemy.CardDefs.BrethrenOfAsh.FlyingTerrors where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

batHorror :: CardDef
batHorror =
  (enemy "12162" "Bat Horror" FlyingTerrors 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
    }
