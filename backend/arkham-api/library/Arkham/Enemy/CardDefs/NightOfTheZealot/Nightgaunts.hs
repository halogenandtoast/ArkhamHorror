module Arkham.Enemy.CardDefs.NightOfTheZealot.Nightgaunts where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

huntingNightgaunt :: CardDef
huntingNightgaunt =
  (enemy "01172" "Hunting Nightgaunt" Nightgaunts 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
