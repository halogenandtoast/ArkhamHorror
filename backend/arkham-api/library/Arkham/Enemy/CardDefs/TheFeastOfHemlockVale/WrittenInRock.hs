module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.WrittenInRock where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

burrowingHybrid :: CardDef
burrowingHybrid =
  (enemy "10518" "Burrowing Hybrid" WrittenInRock 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Mutated]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

frenziedMiner :: CardDef
frenziedMiner =
  (enemy "10519" "Frenzied Miner" WrittenInRock 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fightX
    , cdEvade = evadeX
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

subterraneanBeast :: CardDef
subterraneanBeast =
  (enemy "10517" "Subterranean Beast" WrittenInRock 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Abomination, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 2
    }
