module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.DayOfTheFeast where

import Arkham.Enemy.CardDefs.Import

frenziedReveler :: CardDef
frenziedReveler =
  (enemy "10692" "Frenzied Reveler" DayOfTheFeast 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Resident]
    }
