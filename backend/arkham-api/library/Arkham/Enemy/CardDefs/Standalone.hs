{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.Standalone where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

tommyMalloy :: CardDef
tommyMalloy =
  unique
    $ (weakness "60103" "Tommy Malloy")
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

vengefulShade :: CardDef
vengefulShade =
  (weakness "90053" "Vengeful Shade")
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = singleton Keyword.Hunter
    }

serpentsOfYigAdvanced :: CardDef
serpentsOfYigAdvanced =
  (weakness "90083" "Serpents of Yig")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Advanced]
    , cdRevelation = IsRevelation
    }

felineHybrid :: CardDef
felineHybrid =
  unique
    $ (weakness "60553" "Feline Hybrid")
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Creature, Mutated]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
      }

bloodDrinker :: CardDef
bloodDrinker =
  (basicWeakness "60554" "Blood Drinker")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

-- The Blob That Ate Everything

-- Mi-Go Incursion

replicatingAberrationA :: CardDef
replicatingAberrationB :: CardDef
replicatingAberrationC :: CardDef
replicatingAberrationD :: CardDef
replicatingAberrationE :: CardDef
replicatingAberrationF :: CardDef
replicatingAberrationG :: CardDef
replicatingAberrationH :: CardDef
replicatingAberrationI :: CardDef
replicatingAberrationA = replicatingAberration "89010a"
replicatingAberrationB = replicatingAberration "89010b"
replicatingAberrationC = replicatingAberration "89010c"
replicatingAberrationD = replicatingAberration "89010d"
replicatingAberrationE = replicatingAberration "89010e"
replicatingAberrationF = replicatingAberration "89010f"
replicatingAberrationG = replicatingAberration "89010g"
replicatingAberrationH = replicatingAberration "89010h"
replicatingAberrationI = replicatingAberration "89010i"
