module Arkham.Enemy.CardDefs.TheScarletKeys.Outsiders where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

apocalypticPresage :: CardDef
apocalypticPresage =
  (enemy "09732" "Apocalyptic Presage" Outsiders 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdVictoryPoints = Just 1
    }

paracausalEntity :: CardDef
paracausalEntity =
  (enemy "09731" "Paracausal Entity" Outsiders 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = singleton Keyword.Hunter
    }
