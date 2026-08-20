module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheThingInTheDepths where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

chelydranHybrid :: CardDef
chelydranHybrid =
  (enemy "10601" ("Chelydran Hybrid" <:> "Flowering Anomaly") TheThingInTheDepths 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Flora, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Elusive, Keyword.Patrol EmptyLocation]
    , cdVictoryPoints = Just 0
    }

graspingTendril :: CardDef
graspingTendril =
  (enemy "10602" "Grasping Tendril" TheThingInTheDepths 5)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = healthX
    , cdCardTraits = setFromList [Abomination, Flora, Mutated]
    , cdKeywords =
        setFromList
          [ Keyword.Surge
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Aloof
          ]
    }

thingInTheDepths :: CardDef
thingInTheDepths =
  (enemy "10600" ("Thing in the Depths" <:> "Rising from the Deep") TheThingInTheDepths 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 10
    , cdCardTraits = setFromList [Abomination, Flora, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }
