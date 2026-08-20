module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheSilentHeath where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

broodQueenDyingMother :: CardDef
broodQueenDyingMother =
  (enemy "10565" ("Brood Queen" <:> "Dying Mother") TheSilentHeath 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Insect, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Alert]
    , cdVictoryPoints = Just 2
    }

broodSoldier :: CardDef
broodSoldier =
  (enemy "10564" "Brood Soldier" TheSilentHeath 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Insect, Mutated]
    , cdKeywords =
        setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTrait Cave <> LocationWithAnyClues)]
    , cdVictoryPoints = Just 0
    }

colorlessLarva :: CardDef
colorlessLarva =
  (enemy "10563" "Colorless Larva" TheSilentHeath 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Insect, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof]
    , cdVictoryPoints = Just 0
    }
