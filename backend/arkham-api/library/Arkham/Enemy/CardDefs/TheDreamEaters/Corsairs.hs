module Arkham.Enemy.CardDefs.TheDreamEaters.Corsairs where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

corsairOfLeng :: CardDef
corsairOfLeng =
  (enemy "06105" "Corsair of Leng" Corsairs 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 5
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = singleton Keyword.Alert
    }
