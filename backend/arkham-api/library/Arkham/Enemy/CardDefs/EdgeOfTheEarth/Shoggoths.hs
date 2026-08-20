module Arkham.Enemy.CardDefs.EdgeOfTheEarth.Shoggoths where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

forgottenShoggoth :: CardDef
forgottenShoggoth =
  (enemy "08710" "Forgotten Shoggoth" Shoggoths 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

rampagingShoggoth :: CardDef
rampagingShoggoth =
  (enemy "08711" "Rampaging Shoggoth" Shoggoths 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Shoggoth, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }
