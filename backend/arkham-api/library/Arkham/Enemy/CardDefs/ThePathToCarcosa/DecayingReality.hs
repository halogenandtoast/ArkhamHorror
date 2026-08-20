module Arkham.Enemy.CardDefs.ThePathToCarcosa.DecayingReality where

import Arkham.Enemy.CardDefs.Import

maggotSwarm :: CardDef
maggotSwarm =
  (enemy "52068" "Maggot Swarm" DecayingReality 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature]
    }
