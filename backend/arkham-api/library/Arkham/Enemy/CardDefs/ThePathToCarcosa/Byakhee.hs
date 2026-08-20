module Arkham.Enemy.CardDefs.ThePathToCarcosa.Byakhee where

import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

swiftByakhee :: CardDef
swiftByakhee =
  (enemy "03086" "Swift Byakhee" EncounterSet.Byakhee 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
