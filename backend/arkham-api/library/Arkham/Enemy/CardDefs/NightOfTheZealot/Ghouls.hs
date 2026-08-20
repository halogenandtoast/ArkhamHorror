module Arkham.Enemy.CardDefs.NightOfTheZealot.Ghouls where

import Arkham.Enemy.CardDefs.Import

ghoulMinion :: CardDef
ghoulMinion =
  (enemy "01160" "Ghoul Minion" Ghouls 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

ravenousGhoul :: CardDef
ravenousGhoul =
  (enemy "01161" "Ravenous Ghoul" Ghouls 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }
