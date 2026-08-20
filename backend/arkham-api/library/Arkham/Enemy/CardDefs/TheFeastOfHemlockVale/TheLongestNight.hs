module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheLongestNight where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

capraHybrid :: CardDef
capraHybrid =
  (enemy "10646" "Capra Hybrid" TheLongestNight 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

equineHybridA :: CardDef
equineHybridA =
  (enemy "10647a" "Equine Hybrid" TheLongestNight 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

equineHybridB :: CardDef
equineHybridB =
  (enemy "10647b" "Equine Hybrid" TheLongestNight 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

equineHybridC :: CardDef
equineHybridC =
  (enemy "10647c" "Equine Hybrid" TheLongestNight 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

lupineHybridA :: CardDef
lupineHybridA =
  (enemy "10645a" "Lupine Hybrid" TheLongestNight 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

lupineHybridB :: CardDef
lupineHybridB =
  (enemy "10645b" "Lupine Hybrid" TheLongestNight 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

lupineHybridC :: CardDef
lupineHybridC =
  (enemy "10645c" "Lupine Hybrid" TheLongestNight 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

moltingHybridA :: CardDef
moltingHybridA =
  (enemy "10644a" "Molting Hybrid" TheLongestNight 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

moltingHybridB :: CardDef
moltingHybridB =
  (enemy "10644b" "Molting Hybrid" TheLongestNight 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

moltingHybridC :: CardDef
moltingHybridC =
  (enemy "10644c" "Molting Hybrid" TheLongestNight 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

slitheringHybrid :: CardDef
slitheringHybrid =
  (enemy "10648" "Slithering Hybrid" TheLongestNight 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

ursineHybridStarvingAbomination :: CardDef
ursineHybridStarvingAbomination =
  (enemy "10643" ("Ursine Hybrid" <:> "Starving Abomination") TheLongestNight 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 6
    , cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    , cdMeta = longestNightBack
    }
