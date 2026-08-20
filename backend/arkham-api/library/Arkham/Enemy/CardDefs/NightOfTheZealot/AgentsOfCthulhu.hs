module Arkham.Enemy.CardDefs.NightOfTheZealot.AgentsOfCthulhu where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

youngDeepOne :: CardDef
youngDeepOne =
  (enemy "01181" "Young Deep One" AgentsOfCthulhu 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
