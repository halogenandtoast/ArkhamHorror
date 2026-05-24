module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

zamacona :: CardDef
zamacona =
  (weakness "10011" "Zamacona")
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Elusive]
    }

weepingYurei :: CardDef
weepingYurei =
  (weakness "10014" "Weeping Yurei")
    { cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Elusive, Keyword.Hunter]
    }

biancaDieKatz :: CardDef
biancaDieKatz =
  (weakness "10063" "Bianca \"Die Katz\"")
    { cdCardTraits = setFromList [Humanoid, Criminal, Socialite]
    , cdKeywords = setFromList [Keyword.Bonded 1 "10062", Keyword.Hunter]
    , cdVictoryPoints = Just 0
    }

subterraneanBeast :: CardDef
subterraneanBeast =
  (enemy "10517" "Subterranean Beast" WrittenInRock 1)
    { cdCardTraits = setFromList [Abomination, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 2
    }

burrowingHybrid :: CardDef
burrowingHybrid =
  (enemy "10518" "Burrowing Hybrid" WrittenInRock 3)
    { cdCardTraits = setFromList [Creature, Mutated]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

frenziedMiner :: CardDef
frenziedMiner =
  (enemy "10519" "Frenzied Miner" WrittenInRock 3)
    { cdCardTraits = setFromList [Humanoid]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

grapplingSpawn :: CardDef
grapplingSpawn =
  (enemy "10544" "Grappling Spawn" HemlockHouse 2)
    { cdCardTraits = setFromList [Monster, Mutated]
    , cdKeywords =
        setFromList
          [Keyword.Hunter, Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Retaliate]
    }

colorlessLarva :: CardDef
colorlessLarva =
  (enemy "10563" "Colorless Larva" TheSilentHeath 3)
    { cdCardTraits = setFromList [Creature, Insect, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof]
    , cdVictoryPoints = Just 0
    }

broodSoldier :: CardDef
broodSoldier =
  (enemy "10564" "Brood Soldier" TheSilentHeath 3)
    { cdCardTraits = setFromList [Creature, Insect, Mutated]
    , cdKeywords =
        setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTrait Cave <> LocationWithAnyClues)]
    , cdVictoryPoints = Just 0
    }

broodQueenDyingMother :: CardDef
broodQueenDyingMother =
  (enemy "10565" ("Brood Queen" <:> "Dying Mother") TheSilentHeath 1)
    { cdCardTraits = setFromList [Creature, Insect, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Alert]
    , cdVictoryPoints = Just 2
    }

ursineHybridGlowingAbomination :: CardDef
ursineHybridGlowingAbomination =
  doubleSided "10607a"
    $ (enemy "10607b" ("Ursine Hybrid" <:> "Glowing Abomination") TheTwistedHollow 1)
      { cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdUnique = True
      , cdVictoryPoints = Just 2
      }

stalkingHybrid :: CardDef
stalkingHybrid =
  (enemy "10625" "Stalking Hybrid" TheTwistedHollow 4)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    }

crystalParasite :: CardDef
crystalParasite =
  (enemy "10721" "Crystal Parasite" HorrorsInTheRock 2)
    { cdCardTraits = setFromList [Monster, Insect, Blight]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

miasmaticShadow :: CardDef
miasmaticShadow =
  (enemy "10724" "Miasmatic Shadow" AgentsOfTheColour 2)
    { cdCardTraits = setFromList [Monster, Colour]
    , cdKeywords =
        setFromList
          [ Keyword.Aloof
          , Keyword.Hunter
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Elusive
          ]
    , cdVictoryPoints = Just 0
    }

poisonblossom :: CardDef
poisonblossom =
  (enemy "10732" "Poisonblossom" TheForest 2)
    { cdCardTraits = setFromList [Creature, Flora, Mutated]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

forestWatcher :: CardDef
forestWatcher =
  (enemy "10733" "Forest Watcher" TheForest 2)
    { cdCardTraits = setFromList [Creature, Flora, Mutated]
    , cdKeywords =
        setFromList [Keyword.Aloof, Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Elusive]
    }

cochlealStag :: CardDef
cochlealStag =
  (enemy "10734" "Cochleal Stag" TheForest 1)
    { cdCardTraits = setFromList [Creature, Monster, Flora, Mutated]
    , cdKeywords =
        setFromList
          [Keyword.Elusive, Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Hunter]
    }

blackAmanita :: CardDef
blackAmanita =
  (enemy "10738" "Black Amanita" Myconids 2)
    { cdCardTraits = setFromList [Creature, Monster, Flora, Mutated]
    , cdKeywords =
        setFromList
          [ Keyword.ScenarioModifierKeyword "time" (String "Day") Keyword.Aloof
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Massive
          ]
    }

corpseLichen :: CardDef
corpseLichen =
  (enemy "10739" "Corpse Lichen" Myconids 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Flora, Mutated]
    , cdKeywords =
        setFromList
          [ Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Hunter
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Alert
          ]
    }
