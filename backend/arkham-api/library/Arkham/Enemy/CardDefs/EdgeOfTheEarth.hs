module Arkham.Enemy.CardDefs.EdgeOfTheEarth where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mobGoons :: CardDef
mobGoons =
  (weakness "08003" "Mob Goons")
    { cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = singleton Keyword.Hunter
    }

skitteringNonsense :: CardDef
skitteringNonsense =
  (enemy "08515" "Skittering Nonsense" IceAndDeath 3)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

terrorOfTheStarsBringerOfIceAndDeath :: CardDef
terrorOfTheStarsBringerOfIceAndDeath =
  (enemy "08522" ("Terror of the Stars" <:> "Bringer of Ice and Death") TheCrash 1)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

professorWilliamDyerProfessorOfGeology :: CardDef
professorWilliamDyerProfessorOfGeology =
  (enemy "08535" ("Professor William Dyer" <:> "Professor of Geology") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

danforthBrilliantStudent :: CardDef
danforthBrilliantStudent =
  (enemy "08536" ("Danforth" <:> "Brilliant Student") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

eliyahAshevakDogHandler :: CardDef
eliyahAshevakDogHandler =
  (enemy "08537" ("Eliyah Ashevak" <:> "Dog Handler") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

drMalaSinhaDaringPhysician :: CardDef
drMalaSinhaDaringPhysician =
  (enemy "08538" ("Dr. Mala Sinha" <:> "Daring Physician") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

averyClaypoolAntarcticGuide :: CardDef
averyClaypoolAntarcticGuide =
  (enemy "08539" ("Avery Claypool" <:> "Antarctic Guide") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

jamesCookieFredericksDubiousChoice :: CardDef
jamesCookieFredericksDubiousChoice =
  (enemy "08540" ("James \"Cookie\" Fredericks" <:> "Dubious Choice") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdKeywords = singleton Keyword.Retaliate
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

drAmyKenslerProfessorOfBiology :: CardDef
drAmyKenslerProfessorOfBiology =
  (enemy "08541" ("Dr. Amy Kensler" <:> "Professor of Biology") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

roaldEllsworthIntrepidExplorer :: CardDef
roaldEllsworthIntrepidExplorer =
  (enemy "08542" ("Roald Ellsworth" <:> "Intrepid Explorer") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

takadaHirokoAeroplaneMechanic :: CardDef
takadaHirokoAeroplaneMechanic =
  (enemy "08543" ("Takada Hiroko" <:> "Aeroplane Mechanic") LostInTheNight 1)
    { cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

seepingNightmare :: CardDef
seepingNightmare =
  (enemy "08547" "Seeping Nightmare" SeepingNightmares 4)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    }

memoryOfAHuntGoneAwry :: CardDef
memoryOfAHuntGoneAwry =
  doubleSided "08575b"
    $ (enemy "08575" "Memory of a Hunt Gone Awry" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      }

memoryOfALostPatient :: CardDef
memoryOfALostPatient =
  doubleSided "08576b"
    $ (enemy "08576" "Memory of a Lost Patient" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfAMissingFather :: CardDef
memoryOfAMissingFather =
  doubleSided "08577b"
    $ (enemy "08577" "Memory of a Missing Father" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfARavagedCountry :: CardDef
memoryOfARavagedCountry =
  doubleSided "08578b"
    $ (enemy "08578" "Memory of a Ravaged Country" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfARegretfulVoyage :: CardDef
memoryOfARegretfulVoyage =
  doubleSided "08579b"
    $ (enemy "08579" "Memory of a Regretful Voyage" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfAnUnspeakableEvil :: CardDef
memoryOfAnUnspeakableEvil =
  doubleSided "08580b"
    $ (enemy "08580" "Memory of an Unspeakable Evil" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      }

memoryOfATerribleDiscovery :: CardDef
memoryOfATerribleDiscovery =
  doubleSided "08581b"
    $ (enemy "08581" "Memory of a Terrible Discovery" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

memoryOfAnAlienTranslation :: CardDef
memoryOfAnAlienTranslation =
  doubleSided "08582b"
    $ (enemy "08582" "Memory of an Alien Transformation" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

memoryOfAnUnrequitedLove :: CardDef
memoryOfAnUnrequitedLove =
  doubleSided "08583b"
    $ (enemy "08583" "Memory of an Unrequited Love" FatalMirage 1)
      { cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

horrifyingShade :: CardDef
horrifyingShade =
  (enemy "08584" "Horrifying Shade" FatalMirage 3)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

terrorOfTheStarsGuardianOfForbiddenPeaks :: CardDef
terrorOfTheStarsGuardianOfForbiddenPeaks =
  (enemy "08608" ("Terror of the Stars" <:> "Guardian of the Forbidden Peaks") ToTheForbiddenPeaks 1)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

constrictingElderThing :: CardDef
constrictingElderThing =
  (enemy "08609" "Constricting Elder Thing" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

terrorOfTheStarsBaneOfTheElderThings :: CardDef
terrorOfTheStarsBaneOfTheElderThings =
  (enemy "08641" ("Terror of the Stars" <:> "Bane of the Elder Things") CityOfTheElderThings 1)
    { cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

benignElderThing :: CardDef
benignElderThing =
  (enemy "08642" "Benign Elder Thing" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Monster, ElderThing]
    }

reawakenedElderThing :: CardDef
reawakenedElderThing =
  (enemy "08643" "Reawakened48;46;178;1656;284848;46;178;1656;2848 Elder Thing" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

protoplasmicMass :: CardDef
protoplasmicMass =
  (enemy "08669" "Protoplasmic Mass" TheGreatSeal 2)
    { cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theNamelessMadness :: CardDef
theNamelessMadness =
  (enemy "08679" "The Nameless Madness" StirringInTheDeep 15)
    { cdCardTraits = setFromList [AncientOne, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Retaliate]
    }

unsealedPhantasm :: CardDef
unsealedPhantasm =
  (enemy "08680" "Unsealed Phantasm" StirringInTheDeep 2)
    { cdCardTraits = setFromList [Monster, Eidolon, Shoggoth]
    , cdVictoryPoints = Just 1
    }

primordialEvil :: CardDef
primordialEvil =
  (enemy "08687" "Primordial Evil" AgentsOfTheUnknown 2)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

manifestationOfMadness :: CardDef
manifestationOfMadness =
  (enemy "08689" "Manifestation of Madness" CreaturesInTheIce 3)
    { cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

glacialPhantasm :: CardDef
glacialPhantasm =
  (enemy "08690" "Glacial Phantasm" CreaturesInTheIce 2)
    { cdCardTraits = setFromList [Monster, Eidolon]
    }

elderThingScavenger :: CardDef
elderThingScavenger =
  (enemy "08695" "Elder Thing Scavenger" ElderThings 2)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

guardianElderThing :: CardDef
guardianElderThing =
  (enemy "08696" "Guardian Elder Thing" ElderThings 2)
    { cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

lostResearcher :: CardDef
lostResearcher =
  (enemy "08700" "Lost Researcher" LeftBehind 2)
    { cdCardTraits = setFromList [Humanoid, Possessed]
    }

frenziedExplorer :: CardDef
frenziedExplorer =
  (enemy "08701" "Frenzied Explorer" LeftBehind 2)
    { cdCardTraits = setFromList [Humanoid, Possessed]
    }

giantAlbinoPenguin :: CardDef
giantAlbinoPenguin =
  (enemy "08708" "Giant Albino Penguin" Penguins 2)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

forgottenShoggoth :: CardDef
forgottenShoggoth =
  (enemy "08710" "Forgotten Shoggoth" Shoggoths 2)
    { cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

rampagingShoggoth :: CardDef
rampagingShoggoth =
  (enemy "08711" "Rampaging Shoggoth" Shoggoths 1)
    { cdCardTraits = setFromList [Monster, Shoggoth, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }
