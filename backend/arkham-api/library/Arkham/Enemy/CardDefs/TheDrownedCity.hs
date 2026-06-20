module Arkham.Enemy.CardDefs.TheDrownedCity where

import Arkham.Enemy.CardDefs.Import
import Arkham.EncounterSet qualified as Set
import Arkham.Keyword qualified as Keyword

-- One Last Job
sadieSheldon :: CardDef
sadieSheldon =
  unique
    $ (enemy "11510" ("Sadie Sheldon" <:> "Runs this Town") OneLastJob 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

naomiOBannion :: CardDef
naomiOBannion =
  unique
    $ (enemy "11511" ("Naomi O'Bannion" <:> "Just Doing Business") OneLastJob 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      , cdVictoryPoints = Just 1
      }

gangSoldier :: CardDef
gangSoldier =
  (enemy "11512" "Gang Soldier" OneLastJob 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }

gangEnforcer :: CardDef
gangEnforcer =
  (enemy "11513" "Gang Enforcer" OneLastJob 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

gangInformant :: CardDef
gangInformant =
  (enemy "11514" "Gang Informant" OneLastJob 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

-- The Western Wall
deepOneMatron :: CardDef
deepOneMatron =
  (enemy "11533" "Deep One Matron" TheWesternWall 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 6
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

huntingParasite :: CardDef
huntingParasite =
  unique
    $ (enemy "11535" "Hunting Parasite" TheWesternWall 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster, Stowaway]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      }

-- The Drowned Quarter
seafloorLeviathan :: CardDef
seafloorLeviathan =
  (enemy "11537b" ("Seafloor Leviathan" <:> "Giant Aquatic Medusoid") TheDrownedQuarter 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Patrol (not_ FullyFloodedLocation)]
    , cdVictoryPoints = Just 2
    }

underseaParasite :: CardDef
underseaParasite =
  doubleSided "11549b"
    $ (enemy "11549" "Undersea Parasite" TheDrownedQuarter 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster, Glyph, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }

medusa :: CardDef
medusa =
  (enemy "11551" "Medusa" TheDrownedQuarter 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (not_ FullyFloodedLocation)]
    }

-- The Apiary
mother :: CardDef
mother =
  unique
    $ (enemy "11573" "Mother" TheApiary 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = healthPerInvestigator 8
      , cdCardTraits = setFromList [Abomination, Stowaway, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Relentless]
      , cdVictoryPoints = Just 2
      }

grotesqueAmalgam :: CardDef
grotesqueAmalgam =
  (enemy "11574" "Grotesque Amalgam" TheApiary 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Stowaway]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

apiaryTender :: CardDef
apiaryTender =
  (enemy "11575" "Apiary Tender" TheApiary 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Stowaway]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

squamousParasite :: CardDef
squamousParasite =
  doubleSided "11580b"
    $ (enemy "11580" "Squamous Parasite" TheApiary 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Monster, Glyph]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

-- The Grand Vault
slithererInDarkness :: CardDef
slithererInDarkness =
  unique
    $ (enemy "11605" ("Slitherer in Darkness" <:> "Lurker From Below") TheGrandVault 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Stowaway, Elite]
      , cdVictoryPoints = Just 1
      }

vaultAttendant :: CardDef
vaultAttendant =
  (enemy "11606" "Vault Attendant" TheGrandVault 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Keeper]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

-- Court of the Ancients
courtKeeperObserverOfDreams :: CardDef
courtKeeperObserverOfDreams =
  (enemy "11630" ("Court Keeper" <:> "Observer of Dreams") CourtOfTheAncients 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Keeper, Glyph, Elite]
    , cdVictoryPoints = Just 1
    }

courtKeeperWeaverOfNightmares :: CardDef
courtKeeperWeaverOfNightmares =
  (enemy "11631" ("Court Keeper" <:> "Weaver of Nightmares") CourtOfTheAncients 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Keeper, Glyph, Elite]
    , cdVictoryPoints = Just 1
    }

colossalTyrant :: CardDef
colossalTyrant =
  unique
    $ (enemy "11635" ("Colossal Tyrant" <:> "Trapped in the Tower") CourtOfTheAncients 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      , cdVictoryPoints = Just 1
      }

wingedKeeper :: CardDef
wingedKeeper =
  (enemy "11637" "Winged Keeper" CourtOfTheAncients 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Keeper]
    , cdKeywords = setFromList [Keyword.Alert]
    }

-- Obsidian Canyons
primevalTerror :: CardDef
primevalTerror =
  (enemy "11670" "Primeval Terror" ObsidianCanyons 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster]
    }

starVampire :: CardDef
starVampire =
  (enemy "11671" "Star Vampire" ObsidianCanyons 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Relentless]
    , cdVictoryPoints = Just 1
    }

-- Sepulchre of the Sleeper
cthulhuDeadAndDreaming :: CardDef
cthulhuDeadAndDreaming =
  unique
    $ (enemy "11674b" ("Cthulhu" <:> "Dead and Dreaming") SepulchreOfTheSleeper 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 20
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Relentless]
      , cdVictoryPoints = Just 5
      }

-- The Doom of Arkham, Part I
randallTillinghast :: CardDef
randallTillinghast =
  unique
    $ (enemy "11686" ("Randall Tillinghast" <:> "Out for Your Blood") TheDoomOfArkhamPartI 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Relentless, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

-- The Doom of Arkham, Part II
cthulhuAncientEvil :: CardDef
cthulhuAncientEvil =
  unique
    $ (enemy "11701" ("Cthulhu" <:> "Ancient Evil") TheDoomOfArkhamPartII 1)
      { cdFight = fightStar
      , cdEvade = evadeStar
      , cdHealth = healthStar
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      }

cthulhuHoaryWings :: CardDef
cthulhuHoaryWings =
  doubleSided "11702b"
    $ (enemy "11702" ("Cthulhu" <:> "Hoary Wings") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdCardTraits = setFromList [Cthulhu, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

cthulhuHoaryWingsEnraged :: CardDef
cthulhuHoaryWingsEnraged =
  doubleSided "11702"
    $ (enemy "11702b" ("Cthulhu" <:> "Hoary Wings") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdHealth = healthX
      , cdCardTraits = setFromList [Cthulhu, Enraged, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

cthulhuFierceVisage :: CardDef
cthulhuFierceVisage =
  doubleSided "11703b"
    $ (enemy "11703" ("Cthulhu" <:> "Fierce Visage") TheDoomOfArkhamPartII 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdCardTraits = setFromList [Cthulhu, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 5
      , cdUnique = True
      }

cthulhuFierceVisageEnraged :: CardDef
cthulhuFierceVisageEnraged =
  doubleSided "11703"
    $ (enemy "11703b" ("Cthulhu" <:> "Fierce Visage") TheDoomOfArkhamPartII 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdHealth = healthX
      , cdCardTraits = setFromList [Cthulhu, Enraged, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 5
      , cdUnique = True
      }

cthulhuWickedClaw :: CardDef
cthulhuWickedClaw =
  doubleSided "11704b"
    $ (enemy "11704" ("Cthulhu" <:> "Wicked Claw") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdCardTraits = setFromList [Cthulhu, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

cthulhuWickedClawEnraged :: CardDef
cthulhuWickedClawEnraged =
  doubleSided "11704"
    $ (enemy "11704b" ("Cthulhu" <:> "Wicked Claw") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdHealth = healthX
      , cdCardTraits = setFromList [Cthulhu, Enraged, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

-- Stowaways
stowawayDrone :: CardDef
stowawayDrone =
  (enemy "11721" "Stowaway Drone" Stowaways 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Stowaway]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

-- Pilgrims
pilgrimAcolyte :: CardDef
pilgrimAcolyte =
  (enemy "11723" "Pilgrim Acolyte" Pilgrims 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

pilgrimLeader :: CardDef
pilgrimLeader =
  (enemy "11724" "Pilgrim Leader" Pilgrims 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

-- Star Spawn
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

-- Undersea Creatures
voltaicEel :: CardDef
voltaicEel =
  (enemy "11733" "Voltaic Eel" UnderseaCreatures 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

-- The Inescapable
theInescapable :: CardDef
theInescapable =
  unique
    $ (enemy "11744" ("The Inescapable" <:> "Tireless Pursuer") TheInescapable 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, StarSpawn, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Relentless]
      , cdVictoryPoints = Just 0
      }

-- Deep Ones
deepOneThrall :: CardDef
deepOneThrall =
  (enemy "11746" "Deep One Thrall" DeepOnes 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }

elderDeepOne :: CardDef
elderDeepOne =
  (enemy "11747" "Elder Deep One" DeepOnes 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

-- Alien Machinery
persistentConstruct :: CardDef
persistentConstruct =
  (enemy "11751" "Persistent Construct" AlienMachinery 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Construct]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    }
