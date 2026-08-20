module Arkham.Enemy.CardDefs.TheDrownedCity.StarSpawn where

import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

coralStarSpawn :: CardDef
coralStarSpawn =
  (enemy "11727" "Coral Star Spawn" Set.StarSpawn 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, StarSpawn, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Relentless]
    , cdVictoryPoints = Just 1
    }

infectedStarSpawn :: CardDef
infectedStarSpawn =
  (enemy "11726" "Infected Star Spawn" Set.StarSpawn 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, StarSpawn, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Relentless]
    , cdVictoryPoints = Just 1
    }

monstrousStarSpawn :: CardDef
monstrousStarSpawn =
  (enemy "11725" "Monstrous Star Spawn" Set.StarSpawn 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, StarSpawn, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Relentless]
    , cdVictoryPoints = Just 1
    }

starSpawnObserver :: CardDef
starSpawnObserver =
  (enemy "11728" "Star Spawn Observer" Set.StarSpawn 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, StarSpawn, Elite]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
