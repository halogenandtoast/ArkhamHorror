module Arkham.Enemy.CardDefs.EdgeOfTheEarth.LeftBehind where

import Arkham.Enemy.CardDefs.Import

frenziedExplorer :: CardDef
frenziedExplorer =
  (enemy "08701" "Frenzied Explorer" LeftBehind 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Possessed]
    }

lostResearcher :: CardDef
lostResearcher =
  (enemy "08700" "Lost Researcher" LeftBehind 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Possessed]
    }
