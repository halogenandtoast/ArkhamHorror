module Arkham.Enemy.CardDefs.ReturnToNightOfTheZealot.ReturnToTheGathering where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

corpseHungryGhoul :: CardDef
corpseHungryGhoul =
  (enemy "50022" "Corpse-Hungry Ghoul" ReturnToTheGathering 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

ghoulFromTheDepths :: CardDef
ghoulFromTheDepths =
  (enemy "50023" "Ghoul from the Depths" ReturnToTheGathering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }
