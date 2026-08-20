module Arkham.Enemy.CardDefs.EdgeOfTheEarth.IceAndDeath where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

averyClaypoolAntarcticGuide :: CardDef
averyClaypoolAntarcticGuide =
  (enemy "08539" ("Avery Claypool" <:> "Antarctic Guide") LostInTheNight 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

danforthBrilliantStudent :: CardDef
danforthBrilliantStudent =
  (enemy "08536" ("Danforth" <:> "Brilliant Student") LostInTheNight 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

drAmyKenslerProfessorOfBiology :: CardDef
drAmyKenslerProfessorOfBiology =
  (enemy "08541" ("Dr. Amy Kensler" <:> "Professor of Biology") LostInTheNight 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

drMalaSinhaDaringPhysician :: CardDef
drMalaSinhaDaringPhysician =
  (enemy "08538" ("Dr. Mala Sinha" <:> "Daring Physician") LostInTheNight 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

eliyahAshevakDogHandler :: CardDef
eliyahAshevakDogHandler =
  (enemy "08537" ("Eliyah Ashevak" <:> "Dog Handler") LostInTheNight 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

jamesCookieFredericksDubiousChoice :: CardDef
jamesCookieFredericksDubiousChoice =
  (enemy "08540" ("James \"Cookie\" Fredericks" <:> "Dubious Choice") LostInTheNight 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdKeywords = singleton Keyword.Retaliate
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

professorWilliamDyerProfessorOfGeology :: CardDef
professorWilliamDyerProfessorOfGeology =
  (enemy "08535" ("Professor William Dyer" <:> "Professor of Geology") LostInTheNight 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

roaldEllsworthIntrepidExplorer :: CardDef
roaldEllsworthIntrepidExplorer =
  (enemy "08542" ("Roald Ellsworth" <:> "Intrepid Explorer") LostInTheNight 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

seepingNightmare :: CardDef
seepingNightmare =
  (enemy "08547" "Seeping Nightmare" SeepingNightmares 4)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Eidolon, Elite]
    }

skitteringNonsense :: CardDef
skitteringNonsense =
  (enemy "08515" "Skittering Nonsense" IceAndDeath 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

takadaHirokoAeroplaneMechanic :: CardDef
takadaHirokoAeroplaneMechanic =
  (enemy "08543" ("Takada Hiroko" <:> "Aeroplane Mechanic") LostInTheNight 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

terrorOfTheStarsBringerOfIceAndDeath :: CardDef
terrorOfTheStarsBringerOfIceAndDeath =
  (enemy "08522" ("Terror of the Stars" <:> "Bringer of Ice and Death") TheCrash 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }
