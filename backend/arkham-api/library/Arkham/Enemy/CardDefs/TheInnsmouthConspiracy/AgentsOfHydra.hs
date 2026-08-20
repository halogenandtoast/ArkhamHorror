module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.AgentsOfHydra where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

lloigor :: CardDef
lloigor =
  (enemy "07086" "Lloigor" AgentsOfHydra 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
