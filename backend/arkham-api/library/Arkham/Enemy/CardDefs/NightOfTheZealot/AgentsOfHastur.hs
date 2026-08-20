module Arkham.Enemy.CardDefs.NightOfTheZealot.AgentsOfHastur where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

screechingByakhee :: CardDef
screechingByakhee =
  (enemy "01175" "Screeching Byakhee" AgentsOfHastur 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
