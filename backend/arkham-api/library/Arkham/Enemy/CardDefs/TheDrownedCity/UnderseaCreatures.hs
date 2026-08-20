module Arkham.Enemy.CardDefs.TheDrownedCity.UnderseaCreatures where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

voltaicEel :: CardDef
voltaicEel =
  (enemy "11733" "Voltaic Eel" UnderseaCreatures 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
