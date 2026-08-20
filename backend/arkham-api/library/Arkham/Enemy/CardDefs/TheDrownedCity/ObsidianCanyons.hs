module Arkham.Enemy.CardDefs.TheDrownedCity.ObsidianCanyons where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

primevalTerror :: CardDef
primevalTerror =
  (enemy "11670" "Primeval Terror" ObsidianCanyons 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = singleton $ Keyword.Patrol $ LocationWithTrait Summit <> EmptyLocation
    }

starVampire :: CardDef
starVampire =
  (enemy "11671" "Star Vampire" ObsidianCanyons 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Relentless]
    , cdVictoryPoints = Just 1
    }
