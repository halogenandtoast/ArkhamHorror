module Arkham.Enemy.CardDefs.TheScarletKeys.AgentsOfTheOutside where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

paradigmEfficer :: CardDef
paradigmEfficer =
  (enemy "09737" "Paradigm Efficer" AgentsOfTheOutside 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }
