module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.AgentsOfDagon where

import Arkham.Enemy.CardDefs.Import

initiateOfDagon :: CardDef
initiateOfDagon =
  (enemy "07085" "Initiate of Dagon" AgentsOfDagon 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Hybrid, Cultist]
    }

priestOfDagon :: CardDef
priestOfDagon =
  (enemy "07084" "Priest of Dagon" AgentsOfDagon 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }
