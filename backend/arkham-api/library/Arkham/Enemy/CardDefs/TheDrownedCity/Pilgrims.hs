module Arkham.Enemy.CardDefs.TheDrownedCity.Pilgrims where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

pilgrimAcolyte :: CardDef
pilgrimAcolyte =
  (enemy "11723" "Pilgrim Acolyte" Pilgrims 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

pilgrimLeader :: CardDef
pilgrimLeader =
  (enemy "11724" "Pilgrim Leader" Pilgrims 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }
