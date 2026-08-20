module Arkham.Enemy.CardDefs.TheDreamEaters.CreaturesOfTheUnderworld where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

huntingGhast :: CardDef
huntingGhast =
  (enemy "06091" "Hunting Ghast" CreaturesOfTheUnderworld 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Ghast]
    , cdKeywords = singleton Keyword.Hunter
    }

lumberingGug :: CardDef
lumberingGug =
  (enemy "06092" "Lumbering Gug" CreaturesOfTheUnderworld 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Gug]
    }
