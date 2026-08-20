module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.HemlockHouse where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

grapplingSpawn :: CardDef
grapplingSpawn =
  (enemy "10544" "Grappling Spawn" HemlockHouse 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Mutated]
    , cdKeywords =
        setFromList
          [Keyword.Hunter, Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Retaliate]
    }
