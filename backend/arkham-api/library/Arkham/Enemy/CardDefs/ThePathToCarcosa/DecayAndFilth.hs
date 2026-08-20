module Arkham.Enemy.CardDefs.ThePathToCarcosa.DecayAndFilth where

import Arkham.Enemy.CardDefs.Import

roachSwarm :: CardDef
roachSwarm =
  (enemy "03103" "Roach Swarm" DecayAndFilth 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = singleton Creature
    }
