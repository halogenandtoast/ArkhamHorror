module Arkham.Enemy.CardDefs.NightOfTheZealot.GhoulsOfUmordhoth where

import Arkham.Enemy.CardDefs.Import

acolyteOfUmordhoth :: CardDef
acolyteOfUmordhoth =
  (enemy "50039" "Acolyte of Umôrdhoth" GhoulsOfUmordhoth 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

graveEater :: CardDef
graveEater =
  (enemy "50038" "Grave-Eater" GhoulsOfUmordhoth 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }
