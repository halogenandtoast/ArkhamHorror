{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

shadowAgents :: CardDef
shadowAgents =
  (weakness "07011" "Shadow Agents")
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Hunter
    }

accursedFollower :: CardDef
accursedFollower =
  (basicWeakness "07038" "Accursed Follower")
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist, Cursed]
    , cdKeywords = singleton Keyword.Aloof
    }

theAmalgam :: CardDef
theAmalgam =
  unique
    $ (enemy "07053" "The Amalgam" ThePitOfDespair 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [Monster, Abomination, DeepOne, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }

angryMob :: CardDef
angryMob =
  doubleSided "07062"
    $ (enemy "07062b" "Angry Mob" TheVanishingOfElinaHarper 1)
      { cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

robertFriendlyDisgruntledDockworker :: CardDef
robertFriendlyDisgruntledDockworker =
  unique
    $ (enemy "07076" ("Robert Friendly" <:> "Disgruntled Dockerworker") TheVanishingOfElinaHarper 1)
      { cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

zadokAllenDrunkAndDisorderly :: CardDef
zadokAllenDrunkAndDisorderly =
  unique
    $ (enemy "07077" ("Zadok Allen" <:> "Drunk and Disorderly") TheVanishingOfElinaHarper 1)
      { cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

brianBurnhamWantsOut :: CardDef
brianBurnhamWantsOut =
  unique
    $ (enemy "07078" ("Brian Burnham" <:> "Wants Out") TheVanishingOfElinaHarper 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

barnabasMarshTheChangeIsUponHim :: CardDef
barnabasMarshTheChangeIsUponHim =
  unique
    $ (enemy "07079" ("Barnabas Marsh" <:> "The Change Is upon Him") TheVanishingOfElinaHarper 1)
      { cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

joyceLittleBookshopOwner :: CardDef
joyceLittleBookshopOwner =
  unique
    $ (enemy "07080" ("Joyce Little" <:> "Bookshop Owner") TheVanishingOfElinaHarper 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

otheraGilmanProprietessOfTheHotel :: CardDef
otheraGilmanProprietessOfTheHotel =
  unique
    $ (enemy "07081" ("Othera Gilman" <:> "Proprietess of the Hotel") TheVanishingOfElinaHarper 1)
      { cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Suspect, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      , cdRevelation = IsRevelation
      }

priestOfDagon :: CardDef
priestOfDagon =
  (enemy "07084" "Priest of Dagon" AgentsOfDagon 1)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

initiateOfDagon :: CardDef
initiateOfDagon =
  (enemy "07085" "Initiate of Dagon" AgentsOfDagon 3)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Hybrid, Cultist]
    }

lloigor :: CardDef
lloigor =
  (enemy "07086" "Lloigor" AgentsOfHydra 1)
    { cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

deepOneBull :: CardDef
deepOneBull =
  (enemy "07088" "Deep One Bull" CreaturesOfTheDeep 1)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }

lurkingDeepOne :: CardDef
lurkingDeepOne =
  (enemy "07089" "Lurking Deep One" CreaturesOfTheDeep 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    }

wingedOneFogOverInnsmouth :: CardDef
wingedOneFogOverInnsmouth =
  (enemy "07094" "Winged One" FogOverInnsmouth 1)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

innsmouthTroublemaker :: CardDef
innsmouthTroublemaker =
  (enemy "07105" "Innsmouth Troublemaker" TheLocals 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Hybrid, Criminal]
    , cdKeywords = singleton Keyword.Hunter
    }

innsmouthShoggoth :: CardDef
innsmouthShoggoth =
  (enemy "07144" "Innsmouth Shoggoth" InTooDeep 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Shoggoth, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

ravagerFromTheDeep :: CardDef
ravagerFromTheDeep =
  (enemy "07145" "Ravager from the Deep" InTooDeep 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

emergingDeepOne :: CardDef
emergingDeepOne =
  (enemy "07146" "Emerging Deep One" InTooDeep 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdRevelation = IsRevelation
    }

theTerrorOfDevilReef_164 :: CardDef
theTerrorOfDevilReef_164 =
  doubleSided "07164"
    $ (enemy "07164b" "The Terror of Devil Reef" DevilReef 1)
      { cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

theTerrorOfDevilReef_165 :: CardDef
theTerrorOfDevilReef_165 =
  doubleSided "07165"
    $ (enemy "07165b" "The Terror of Devil Reef" DevilReef 1)
      { cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      }

deepOnePredator :: CardDef
deepOnePredator =
  (enemy "07182" "Deep One Predator" DevilReef 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = singleton Keyword.Hunter
    }

huntingDeepOne :: CardDef
huntingDeepOne =
  (enemy "07183" "Hunting Deep One" DevilReef 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = singleton Keyword.Hunter
    }

theTerrorOfDevilReefRelentlessMonstrosity :: CardDef
theTerrorOfDevilReefRelentlessMonstrosity =
  doubleSided "07199"
    $ (enemy "07199b" ("The Terror of Devil Reef" <:> "Relentless Monstrosity") HorrorInHighGear 1)
      { cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      }

pursuingMotorcar :: CardDef
pursuingMotorcar =
  (enemy "07213" "Pursuing Motorcar" HorrorInHighGear 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

hitVan :: CardDef
hitVan =
  (enemy "07214" "Hit Van" HorrorInHighGear 2)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

hybridAssassin :: CardDef
hybridAssassin =
  (enemy "07215" "Hybrid Assassin" HorrorInHighGear 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Vehicle, Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

oceirosMarsh :: CardDef
oceirosMarsh =
  (enemy "07253" ("Oceiros Marsh" <:> "Keeper of the Lighthouse") ALightInTheFog 1)
    { cdHealth = health 6
    , cdCardTraits = setFromList [Humanoid, DeepOne, Hybrid, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

deepOneNursemaid :: CardDef
deepOneNursemaid =
  (enemy "07254" "Deep One Nursemaid" ALightInTheFog 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }

deepOneHatchling :: CardDef
deepOneHatchling =
  (enemy "07255" "Deep One Hatchling" ALightInTheFog 4)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Surge]
    }

dagonDeepInSlumber :: CardDef
dagonDeepInSlumber =
  doubleSided "07292b"
    $ (enemy "07292" ("Dagon" <:> "Deep in Slumber") TheLairOfDagon 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdUnique = True
      }

dagonAwakenedAndEnraged :: CardDef
dagonAwakenedAndEnraged =
  doubleSided "07292"
    $ (enemy "07292b" ("Dagon" <:> "Awakened and Enraged") TheLairOfDagon 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      , cdUnique = True
      , cdVictoryPoints = Just 1
      }

apostleOfDagon :: CardDef
apostleOfDagon =
  (enemy "07293" "Apostle of Dagon" TheLairOfDagon 1)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Hybrid, Cultist]
    , cdVictoryPoints = Just 1
    }

cerenerianDeepOne :: CardDef
cerenerianDeepOne =
  (enemy "07294" "Cerenerian Deep One" TheLairOfDagon 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

dagonDeepInSlumberIntoTheMaelstrom :: CardDef
dagonDeepInSlumberIntoTheMaelstrom =
  doubleSided "07330b"
    $ (enemy "07330" ("Dagon" <:> "Deep in Slumber") IntoTheMaelstrom 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdUnique = True
      }

dagonAwakenedAndEnragedIntoTheMaelstrom :: CardDef
dagonAwakenedAndEnragedIntoTheMaelstrom =
  doubleSided "07330"
    $ (enemy "07330b" ("Dagon" <:> "Awakened and Enraged") IntoTheMaelstrom 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdUnique = True
      }

hydraDeepInSlumber :: CardDef
hydraDeepInSlumber =
  doubleSided "07331b"
    $ (enemy "07331" ("Hydra" <:> "Deep in Slumber") IntoTheMaelstrom 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdUnique = True
      }

hydraAwakenedAndEnraged :: CardDef
hydraAwakenedAndEnraged =
  doubleSided "07331"
    $ (enemy "07331b" ("Hydra" <:> "Awakened and Enraged") IntoTheMaelstrom 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdUnique = True
      }

aquaticAbomination :: CardDef
aquaticAbomination =
  (enemy "07332" "Aquatic Abomination" IntoTheMaelstrom 1)
    { cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

dagonsBrood :: CardDef
dagonsBrood =
  (enemy "07333" "Dagon's Brood" IntoTheMaelstrom 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

hydrasBrood :: CardDef
hydrasBrood =
  (enemy "07334" "Hydra's Brood" IntoTheMaelstrom 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
