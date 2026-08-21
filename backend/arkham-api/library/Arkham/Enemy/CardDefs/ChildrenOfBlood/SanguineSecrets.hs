module Arkham.Enemy.CardDefs.ChildrenOfBlood.SanguineSecrets where

import Arkham.Enemy.CardDefs.Import

sanguineCultist :: CardDef
sanguineCultist =
  (enemy "13112" "Sanguine Cultist" SanguineSecrets 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdHealth = health 1
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

bloodCrazedZealot :: CardDef
bloodCrazedZealot =
  (enemy "13113" "Blood-Crazed Zealot" SanguineSecrets 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }
