module Arkham.Enemy.CardDefs.ThePathToCarcosa where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword
import Arkham.EncounterSet qualified as EncounterSet

graveyardGhouls :: CardDef
graveyardGhouls =
  (weakness "03017" "Graveyard Ghouls")
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theThingThatFollows :: CardDef
theThingThatFollows =
  unique
    $ (basicWeakness "03042" "The Thing That Follows")
      { cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Curse]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

theManInThePallidMask :: CardDef
theManInThePallidMask =
  unique
    $ (weakness "03059" "The Man in the Pallid Mask")
      { cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdEncounterSet = Just CurtainCall
      , cdEncounterSetQuantity = Just 1
      }

royalEmissary :: CardDef
royalEmissary =
  unique
    $ (enemy "03060" ("Royal Emissary" <:> "Messenger from Aldebaran") CurtainCall 1)
      { cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords =
          setFromList
            [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

constanceDumaine :: CardDef
constanceDumaine =
  unique
    $ doubleSided "03065"
    $ ( enemy
          "03065b"
          ("Constance Dumaine" <:> "A Little Too Sociable")
          TheLastKing
          1
      )
      { cdHealth = health 6
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

jordanPerry :: CardDef
jordanPerry =
  unique
    $ doubleSided "03066"
    $ (enemy "03066b" ("Jordan Perry" <:> "An Imposing Presence") TheLastKing 1)
      { cdHealth = health 8
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

ishimaruHaruko :: CardDef
ishimaruHaruko =
  unique
    $ doubleSided "03067"
    $ (enemy "03067b" ("Ishimaru Haruko" <:> "Just Skin and Bones") TheLastKing 1)
      { cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

sebastienMoreau :: CardDef
sebastienMoreau =
  unique
    $ doubleSided "03068"
    $ (enemy "03068b" ("Sebastien Moreau" <:> "Savage Hysteria") TheLastKing 1)
      { cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

ashleighClarke :: CardDef
ashleighClarke =
  unique
    $ doubleSided "03069"
    $ (enemy "03069b" ("Ashleigh Clarke" <:> "Songs Die Unheard") TheLastKing 1)
      { cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

dianneDevine :: CardDef
dianneDevine =
  unique
    $ (enemy "03081" ("Dianne Devine" <:> "Mercurial and Mischevious") TheLastKing 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = singleton Keyword.Aloof
      }

swiftByakhee :: CardDef
swiftByakhee =
  (enemy "03086" "Swift Byakhee" EncounterSet.Byakhee 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

beastOfAldebaran :: CardDef
beastOfAldebaran =
  (enemy "03088" "Beast of Aldebaran" InhabitantsOfCarcosa 1)
    { cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

spawnOfHali :: CardDef
spawnOfHali =
  (enemy "03089" "Spawn of Hali" InhabitantsOfCarcosa 2)
    { cdHealth = health 4
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Retaliate
    }

poltergeist :: CardDef
poltergeist =
  (enemy "03093" "Poltergeist" Hauntings 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    }

maniac :: CardDef
maniac =
  (enemy "03095" "Maniac" HastursGift 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }

youngPsychopath :: CardDef
youngPsychopath =
  (enemy "03096" "Young Psychopath" HastursGift 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }

fanatic :: CardDef
fanatic =
  (enemy "03098" "Fanatic" CultOfTheYellowSign 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

agentOfTheKing :: CardDef
agentOfTheKing =
  (enemy "03099" "Agent of the King" CultOfTheYellowSign 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

roachSwarm :: CardDef
roachSwarm =
  (enemy "03103" "Roach Swarm" DecayAndFilth 2)
    { cdHealth = health 2
    , cdCardTraits = singleton Creature
    }

possessedOathspeaker :: CardDef
possessedOathspeaker =
  ( enemy
      "03140"
      ("Possessed Oathspeaker" <:> "A Damnable Fate")
      EchoesOfThePast
      1
  )
    { cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Monster, Servitor, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

seekerOfCarcosa :: CardDef
seekerOfCarcosa =
  (enemy "03144" "Seeker of Carcosa" EchoesOfThePast 3)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Aloof
    }

danielChesterfield :: CardDef
danielChesterfield =
  unique
    $ doubleSided "03182a"
    $ ( enemy
          "03182b"
          ("Daniel Chesterfield" <:> "…Or At Least, What's Left of Him")
          TheUnspeakableOath
          1
      )
      { cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Lunatic, Elite]
      , cdVictoryPoints = Just 1
      }

asylumGorger :: CardDef
asylumGorger =
  (enemy "03183" "Asylum Gorger" TheUnspeakableOath 2)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Hunter
    }

madPatient :: CardDef
madPatient =
  (enemy "03184" "Mad Patient" TheUnspeakableOath 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }

theOrganistHopelessIDefiedHim :: CardDef
theOrganistHopelessIDefiedHim =
  unique
    $ doubleSided "03221b"
    $ ( enemy
          "03221a"
          ("The Organist" <:> "Hopeless, I Defied Him")
          APhantomOfTruth
          1
      )
      { cdCardTraits = setFromList [Humanoid, Avatar, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }

theOrganistDrapedInMystery :: CardDef
theOrganistDrapedInMystery =
  unique
    $ doubleSided "03221a"
    $ (enemy "03221b" ("The Organist" <:> "Draped in Mystery") APhantomOfTruth 1)
      { cdCardTraits = setFromList [Humanoid, Avatar, Elite]
      , cdKeywords = singleton Keyword.Aloof
      }

stealthyByakhee :: CardDef
stealthyByakhee =
  (enemy "03222" "Stealthy Byakhee" APhantomOfTruth 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = singleton Keyword.Hunter
    }

specterOfDeath :: CardDef
specterOfDeath =
  doubleSided "03241a"
    $ (enemy "03241b" ("Specter of Death" <:> "A Force From Beyond") ThePallidMask 1)
      { cdCardTraits = setFromList [Monster, Geist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

catacombsDocent :: CardDef
catacombsDocent =
  (enemy "03258" "Catacombs Docent" ThePallidMask 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }

corpseDweller :: CardDef
corpseDweller =
  (enemy "03259" "Corpse Dweller" ThePallidMask 3)
    { cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

tidalTerror :: CardDef
tidalTerror =
  (enemy "03300" "Tidal Terror" BlackStarsRise 2)
    { cdHealth = health 4
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    }

riftSeeker :: CardDef
riftSeeker =
  (enemy "03301" "Rift Seeker" BlackStarsRise 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee, Cultist]
    }

hasturTheKingInYellow :: CardDef
hasturTheKingInYellow =
  unique
    $ (enemy "03332" ("Hastur" <:> "The King in Yellow") DimCarcosa 1)
      { cdHealth = healthPerInvestigator 7
      , cdCardTraits = setFromList [AncientOne, Elite]
      }

hasturLordOfCarcosa :: CardDef
hasturLordOfCarcosa =
  unique
    $ (enemy "03333" ("Hastur" <:> "Lord of Carcosa") DimCarcosa 1)
      { cdHealth = healthPerInvestigator 9
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

hasturTheTatteredKing :: CardDef
hasturTheTatteredKing =
  unique
    $ (enemy "03334" ("Hastur" <:> "The Tattered King") DimCarcosa 1)
      { cdHealth = healthPerInvestigator 8
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }

creatureOutOfDemhe :: CardDef
creatureOutOfDemhe =
  (enemy "03335" "Creature Out of Demhe" DimCarcosa 1)
    { cdHealth = health 4
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Massive
    }

wingedOne :: CardDef
wingedOne =
  (enemy "03336" "Winged One" DimCarcosa 1)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = singleton Keyword.Retaliate
    }
