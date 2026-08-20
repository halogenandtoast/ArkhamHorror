module Arkham.Enemy.CardDefs.TheDrownedCity.AlienMachinery where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

persistentConstruct :: CardDef
persistentConstruct =
  (enemy "11751" "Persistent Construct" AlienMachinery 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Construct]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    }
