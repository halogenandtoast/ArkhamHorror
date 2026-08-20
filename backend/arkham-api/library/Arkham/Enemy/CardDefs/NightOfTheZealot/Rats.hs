module Arkham.Enemy.CardDefs.NightOfTheZealot.Rats where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

swarmOfRats :: CardDef
swarmOfRats =
  (enemy "01159" "Swarm of Rats" Rats 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
