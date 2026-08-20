module Arkham.Enemy.CardDefs.TheScarletKeys.SecretWar where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

otherworldlyMimic :: CardDef
otherworldlyMimic =
  (enemy "09734" "Otherworldly Mimic" SecretWar 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }
