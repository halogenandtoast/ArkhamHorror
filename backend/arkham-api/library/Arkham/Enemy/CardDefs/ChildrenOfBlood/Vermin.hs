module Arkham.Enemy.CardDefs.ChildrenOfBlood.Vermin where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

bloodCrazedVermin :: CardDef
bloodCrazedVermin =
  (enemy "13117" "Blood-Crazed Vermin" Vermin 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdHealth = health 1
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Creature, Blight]
    , cdKeywords = singleton Keyword.Hunter
    }
