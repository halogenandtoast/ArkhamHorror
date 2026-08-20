module Arkham.Enemy.CardDefs.NightOfTheZealot.TheDevourersCult where

import Arkham.Enemy.CardDefs.Import

corpseTaker :: CardDef
corpseTaker =
  (enemy "50042" "Corpse-Taker" TheDevourersCult 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Servitor, Cultist]
    }

discipleOfTheDevourer :: CardDef
discipleOfTheDevourer =
  (enemy "50041" "Disciple of the Devourer" TheDevourersCult 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }
