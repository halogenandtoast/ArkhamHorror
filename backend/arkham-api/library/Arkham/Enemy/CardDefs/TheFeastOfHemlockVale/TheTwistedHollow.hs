module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheTwistedHollow where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

stalkingHybrid :: CardDef
stalkingHybrid =
  (enemy "10625" "Stalking Hybrid" TheTwistedHollow 4)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = healthX
    , cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    }

ursineHybridGlowingAbomination :: CardDef
ursineHybridGlowingAbomination =
  doubleSided "10607a"
    $ (enemy "10607b" ("Ursine Hybrid" <:> "Glowing Abomination") TheTwistedHollow 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdUnique = True
      , cdVictoryPoints = Just 2
      }
