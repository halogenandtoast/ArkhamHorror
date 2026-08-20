module Arkham.Enemy.CardDefs.ReturnToTheDunwichLegacy.ReturnToBloodOnTheAltar where

import Arkham.Enemy.CardDefs.Import

hiredGun :: CardDef
hiredGun =
  (enemy "51040" "Hired Gun" ReturnToBloodOnTheAltar 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }
