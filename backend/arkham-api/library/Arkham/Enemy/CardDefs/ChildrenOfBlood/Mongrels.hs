module Arkham.Enemy.CardDefs.ChildrenOfBlood.Mongrels where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

bloodCrazedMongrel :: CardDef
bloodCrazedMongrel =
  (enemy "13109" "Blood-Crazed Mongrel" Mongrels 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton Keyword.Hunter
    }
