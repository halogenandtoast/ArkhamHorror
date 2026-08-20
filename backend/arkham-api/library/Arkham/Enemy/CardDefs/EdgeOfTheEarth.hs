module Arkham.Enemy.CardDefs.EdgeOfTheEarth where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mobGoons :: CardDef
mobGoons =
  (weakness "08003" "Mob Goons")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = singleton Keyword.Hunter
    }
