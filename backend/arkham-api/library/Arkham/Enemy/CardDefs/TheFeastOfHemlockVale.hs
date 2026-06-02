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

limulusHybridInTheLight :: CardDef
limulusHybridInTheLight =
  doubleSided "10583b"
    $ (enemy "10583a" ("Limulus Hybrid" <:> "In the Light") TheLostSister 1)
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Massive]
      , cdVictoryPoints = Just 2
      }

limulusHybridInTheDark :: CardDef
limulusHybridInTheDark =
  doubleSided "10583a"
    $ (enemy "10583b" ("Limulus Hybrid" <:> "In the Dark") TheLostSister 1)
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

crustaceanHybridInTheLight :: CardDef
crustaceanHybridInTheLight =
  doubleSided "10584b"
    $ (enemy "10584a" ("Crustacean Hybrid" <:> "In the Light") TheLostSister 2)
      { cdCardTraits = setFromList [Creature, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

crustaceanHybridInTheDark :: CardDef
crustaceanHybridInTheDark =
  doubleSided "10584a"
    $ (enemy "10584b" ("Crustacean Hybrid" <:> "In the Dark") TheLostSister 2)
      { cdCardTraits = setFromList [Creature, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

cavernMoss :: CardDef
cavernMoss =
  (enemy "10585" "Cavern Moss" TheLostSister 3)
    { cdCardTraits = setFromList [Flora, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
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

thingInTheDepths :: CardDef
thingInTheDepths =
  (enemy "10600" ("Thing in the Depths" <:> "Rising from the Deep") TheThingInTheDepths 1)
    { cdCardTraits = setFromList [Abomination, Flora, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

chelydranHybrid :: CardDef
chelydranHybrid =
  (enemy "10601" ("Chelydran Hybrid" <:> "Flowering Anomaly") TheThingInTheDepths 1)
    { cdCardTraits = setFromList [Creature, Flora, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Elusive, Keyword.Patrol EmptyLocation]
    }

graspingTendril :: CardDef
graspingTendril =
  (enemy "10602" "Grasping Tendril" TheThingInTheDepths 5)
    { cdCardTraits = setFromList [Abomination, Flora, Mutated]
    , cdKeywords =
        setFromList
          [ Keyword.Surge
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Aloof
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

longestNightBack :: Map Text Value
longestNightBack = mapFromList [("customBack", String "back_the_longest_night.jpg")]

ursineHybridStarvingAbomination :: CardDef
ursineHybridStarvingAbomination =
  (enemy "10643" ("Ursine Hybrid" <:> "Starving Abomination") TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    , cdMeta = longestNightBack
    }

moltingHybridA :: CardDef
moltingHybridA =
  (enemy "10644a" "Molting Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

moltingHybridB :: CardDef
moltingHybridB =
  (enemy "10644b" "Molting Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

moltingHybridC :: CardDef
moltingHybridC =
  (enemy "10644c" "Molting Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

lupineHybridA :: CardDef
lupineHybridA =
  (enemy "10645a" "Lupine Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

lupineHybridB :: CardDef
lupineHybridB =
  (enemy "10645b" "Lupine Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

lupineHybridC :: CardDef
lupineHybridC =
  (enemy "10645c" "Lupine Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

capraHybrid :: CardDef
capraHybrid =
  (enemy "10646" "Capra Hybrid" TheLongestNight 3)
    { cdCardTraits = setFromList [Creature, Monster, Mutated, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

equineHybridA :: CardDef
equineHybridA =
  (enemy "10647a" "Equine Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

equineHybridB :: CardDef
equineHybridB =
  (enemy "10647b" "Equine Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

equineHybridC :: CardDef
equineHybridC =
  (enemy "10647c" "Equine Hybrid" TheLongestNight 1)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

slitheringHybrid :: CardDef
slitheringHybrid =
  (enemy "10648" "Slithering Hybrid" TheLongestNight 2)
    { cdCardTraits = setFromList [Creature, Monster, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTitle "The Farmhouse")]
    , cdMeta = longestNightBack
    }

frenziedReveler :: CardDef
frenziedReveler =
  (enemy "10692" "Frenzied Reveler" DayOfTheFeast 2)
    { cdCardTraits = setFromList [Humanoid, Resident]
    }

-- Resident enemy sides (the back faces of the Residents asset cards). Each is
-- the flipped, hostile version a resident takes when their Relationship Level
-- is too low during The Final Evening.
motherRachelStarbornHerald :: CardDef
motherRachelStarbornHerald =
  unique
    $ doubleSided "10693"
    $ (enemy "10693b" "Mother Rachel" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

leahAtwood :: CardDef
leahAtwood =
  unique
    $ doubleSided "10694"
    $ (enemy "10694b" "Leah Atwood" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

simeonAtwood :: CardDef
simeonAtwood =
  unique
    $ doubleSided "10695"
    $ (enemy "10695b" "Simeon Atwood" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

williamHemlock :: CardDef
williamHemlock =
  unique
    $ doubleSided "10696"
    $ (enemy "10696b" "William Hemlock" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Elusive]
      , cdVictoryPoints = Just 0
      }

riverHawthorne :: CardDef
riverHawthorne =
  unique
    $ doubleSided "10697"
    $ (enemy "10697b" "River Hawthorne" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

gideonMizrah :: CardDef
gideonMizrah =
  unique
    $ doubleSided "10698"
    $ (enemy "10698b" "Gideon Mizrah" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

judithPark :: CardDef
judithPark =
  unique
    $ doubleSided "10699"
    $ (enemy "10699b" "Judith Park" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

theoPeters :: CardDef
theoPeters =
  unique
    $ doubleSided "10700"
    $ (enemy "10700b" "Theo Peters" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Alert]
      , cdVictoryPoints = Just 0
      }

bertieMusgrave :: CardDef
bertieMusgrave =
  unique
    $ doubleSided "10701"
    $ (enemy "10701b" "Bertie Musgrave" Residents 1)
      { cdCardTraits = setFromList [Humanoid, Miskatonic]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithEnemy (EnemyWithTitle "Mother Rachel"))]
      , cdVictoryPoints = Just 0
      }

cosmicEmissaryTheAbyss :: CardDef
cosmicEmissaryTheAbyss =
  doubleSided "10662b"
    $ (enemy "10662a" ("Cosmic Emissary" <:> "The Abyss") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = singleton Keyword.Massive
      }

cosmicEmissaryTheAbyssShattered :: CardDef
cosmicEmissaryTheAbyssShattered =
  doubleSided "10662a"
    $ (enemy "10662b" ("Cosmic Emissary" <:> "The Abyss") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = singleton Keyword.Massive
      }

cosmicEmissaryThePhantasm :: CardDef
cosmicEmissaryThePhantasm =
  doubleSided "10663b"
    $ (enemy "10663a" ("Cosmic Emissary" <:> "The Phantasm") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
      }

cosmicEmissaryThePhantasmShattered :: CardDef
cosmicEmissaryThePhantasmShattered =
  doubleSided "10663a"
    $ (enemy "10663b" ("Cosmic Emissary" <:> "The Phantasm") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
      }

cosmicEmissaryTheMiasma :: CardDef
cosmicEmissaryTheMiasma =
  doubleSided "10664b"
    $ (enemy "10664a" ("Cosmic Emissary" <:> "The Miasma") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Alert]
      }

cosmicEmissaryTheMiasmaShattered :: CardDef
cosmicEmissaryTheMiasmaShattered =
  doubleSided "10664a"
    $ (enemy "10664b" ("Cosmic Emissary" <:> "The Miasma") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Alert]
      }

cosmicEmissaryTheBrilliance :: CardDef
cosmicEmissaryTheBrilliance =
  doubleSided "10665b"
    $ (enemy "10665a" ("Cosmic Emissary" <:> "The Brilliance") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = singleton Keyword.Massive
      }

cosmicEmissaryTheBrillianceShattered :: CardDef
cosmicEmissaryTheBrillianceShattered =
  doubleSided "10665a"
    $ (enemy "10665b" ("Cosmic Emissary" <:> "The Brilliance") FateOfTheVale 1)
      { cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = singleton Keyword.Massive
      }

crystalMimic :: CardDef
crystalMimic =
  (enemy "10671" "Crystal Mimic" FateOfTheVale 1)
    { cdCardTraits = setFromList [Humanoid, Colour]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
    }
