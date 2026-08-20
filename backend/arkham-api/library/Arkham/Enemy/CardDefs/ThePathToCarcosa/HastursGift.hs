module Arkham.Enemy.CardDefs.ThePathToCarcosa.HastursGift where

import Arkham.Enemy.CardDefs.Import

maniac :: CardDef
maniac =
  (enemy "03095" "Maniac" HastursGift 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }

youngPsychopath :: CardDef
youngPsychopath =
  (enemy "03096" "Young Psychopath" HastursGift 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }
