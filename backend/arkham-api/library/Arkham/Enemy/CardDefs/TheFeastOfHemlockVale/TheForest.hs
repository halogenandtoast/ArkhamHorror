module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheForest where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cochlealStag :: CardDef
cochlealStag =
  (enemy "10734" "Cochleal Stag" TheForest 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Monster, Flora, Mutated]
    , cdKeywords =
        setFromList
          [Keyword.Elusive, Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Hunter]
    }

forestWatcher :: CardDef
forestWatcher =
  (enemy "10733" "Forest Watcher" TheForest 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Flora, Mutated]
    , cdKeywords =
        setFromList [Keyword.Aloof, Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Elusive]
    }

poisonblossom :: CardDef
poisonblossom =
  (enemy "10732" "Poisonblossom" TheForest 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Flora, Mutated]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }
