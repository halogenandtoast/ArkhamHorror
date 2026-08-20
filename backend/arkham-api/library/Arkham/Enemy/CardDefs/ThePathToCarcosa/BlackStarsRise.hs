module Arkham.Enemy.CardDefs.ThePathToCarcosa.BlackStarsRise where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

riftSeeker :: CardDef
riftSeeker =
  (enemy "03301" "Rift Seeker" BlackStarsRise 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee, Cultist]
    }

tidalTerror :: CardDef
tidalTerror =
  (enemy "03300" "Tidal Terror" BlackStarsRise 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    }
