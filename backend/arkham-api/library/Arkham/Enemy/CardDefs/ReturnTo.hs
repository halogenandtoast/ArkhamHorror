{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.ReturnTo where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

corpseHungryGhoul :: CardDef
corpseHungryGhoul =
  (enemy "50022" "Corpse-Hungry Ghoul" ReturnToTheGathering 1)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

ghoulFromTheDepths :: CardDef
ghoulFromTheDepths =
  (enemy "50023" "Ghoul from the Depths" ReturnToTheGathering 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

narogath :: CardDef
narogath =
  unique
    $ doubleSided "50026"
    $ (enemy "50026b" ("Narôgath" <:> "The Charnel Lord") ReturnToTheMidnightMasks 1)
      { cdCardTraits = setFromList [Humanoid, Monster, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }

graveEater :: CardDef
graveEater =
  (enemy "50038" "Grave-Eater" GhoulsOfUmordhoth 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

acolyteOfUmordhoth :: CardDef
acolyteOfUmordhoth =
  (enemy "50039" "Acolyte of Umôrdhoth" GhoulsOfUmordhoth 1)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

discipleOfTheDevourer :: CardDef
discipleOfTheDevourer =
  (enemy "50041" "Disciple of the Devourer" TheDevourersCult 3)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

corpseTaker :: CardDef
corpseTaker =
  (enemy "50042" "Corpse-Taker" TheDevourersCult 1)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Servitor, Cultist]
    }

jeremiahPierce :: CardDef
jeremiahPierce =
  unique
    $ ( enemy
          "50044"
          ("Jeremiah Pierce" <:> "Your Next-Door Neighbor")
          ReturnCultOfUmordhoth
          1
      )
      { cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

billyCooper :: CardDef
billyCooper =
  unique
    $ (enemy "50045" ("Billy Cooper" <:> "The Crooked Cop") ReturnCultOfUmordhoth 1)
      { cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

almaHill :: CardDef
almaHill =
  unique
    $ ( enemy
          "50046"
          ("Alma Hill" <:> "The Inquisitive Historian")
          ReturnCultOfUmordhoth
          1
      )
      { cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      , cdVictoryPoints = Just 1
      }

enthralledSecurityGuard :: CardDef
enthralledSecurityGuard =
  (enemy "51014" "Entralled Security Guard" ReturnToExtracurricularActivities 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

theConductorBeastFromBeyondTheGate :: CardDef
theConductorBeastFromBeyondTheGate =
  doubleSided "51026"
    $ (enemy "51026b" ("The Conductor" <:> "Beast from beyond the Gate") ReturnToTheEssexCountyExpress 1)
      { cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

hiredGun :: CardDef
hiredGun =
  (enemy "51040" "Hired Gun" ReturnToBloodOnTheAltar 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }

broodOfYogSothothChargingBeast :: CardDef
broodOfYogSothothChargingBeast =
  (enemy "51042" ("Brood of Yog-Sothoth" <:> "Charging Beast") ReturnToUndimensionedAndUnseen 1)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothSwellingDevourer :: CardDef
broodOfYogSothothSwellingDevourer =
  (enemy "51043" ("Brood of Yog-Sothoth" <:> "Swelling Devourer") ReturnToUndimensionedAndUnseen 1)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothThrashingSpawn :: CardDef
broodOfYogSothothThrashingSpawn =
  (enemy "51044" ("Brood of Yog-Sothoth" <:> "Thrashing Spawn") ReturnToUndimensionedAndUnseen 1)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

broodOfYogSothothAmorphousTerror :: CardDef
broodOfYogSothothAmorphousTerror =
  (enemy "51045" ("Brood of Yog-Sothoth" <:> "Amorphous Terror") ReturnToUndimensionedAndUnseen 1)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

sethBishopThrallOfYogSothoth :: CardDef
sethBishopThrallOfYogSothoth =
  (enemy "51056" ("Seth Bishop" <:> "Thrall of Yog-Sothoth") ReturnToLostInTimeAndSpace 1)
    { cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Humanoid, Monster, Abomination, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

vassalOfTheLurker :: CardDef
vassalOfTheLurker =
  (enemy "51071" "Vassal of the Lurker" YogSothothsEmissaries 2)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

laComtesseSubverterOfPlans :: CardDef
laComtesseSubverterOfPlans =
  unique
    $ (enemy "52020" ("La Comtesse" <:> "Subverter of Plans") ReturnToCurtainCall 1)
      { cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Servitor]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

dianneDevineKnowsWhatYoureUpTo :: CardDef
dianneDevineKnowsWhatYoureUpTo =
  unique
    $ doubleSided "52023"
    $ (enemy "52023b" ("Dianne Devine" <:> "Knows What You're Up To") ReturnToTheLastKing 1)
      { cdCardTraits = setFromList [Monster, Cultist, Lunatic, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

crazedGuest :: CardDef
crazedGuest =
  doubleSided "52024"
    $ (enemy "52024b" "Crazed Guest" ReturnToTheLastKing 2)
      { cdCardTraits = setFromList [Humanoid, Monster, Lunatic]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

keeperOfTheOath :: CardDef
keeperOfTheOath =
  (enemy "52033" "Keeper of the Oath" ReturnToEchoesOfThePast 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 0
    }

hostOfInsanity :: CardDef
hostOfInsanity =
  (enemy "52037" "Host of Insanity" ReturnToTheUnspeakableOath 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Avatar, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

malformedSkeleton :: CardDef
malformedSkeleton =
  (enemy "52053" "Malformed Skeleton" ReturnToThePallidMask 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

highPriestOfHastur :: CardDef
highPriestOfHastur =
  (enemy "52064" "High Priest of Hastur" ReturnToDimCarcosa 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

maggotSwarm :: CardDef
maggotSwarm =
  (enemy "52068" "Maggot Swarm" DecayingReality 2)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Creature]
    }

preyingByakhee :: CardDef
preyingByakhee =
  (enemy "52069" "Preying Byakhee" HastursEnvoys 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

harbingerOfValusiaTheSleeperReturns :: CardDef
harbingerOfValusiaTheSleeperReturns =
  unique
    $ ( enemy
          "53018"
          ("Harbinger of Valusia" <:> "The Sleeper Returns")
          ReturnToTheDoomOfEztli
          1
      )
      { cdHealth = healthPerInvestigator 10
      , cdCardTraits = setFromList [Humanoid, Serpent, Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
      , cdVengeancePoints = Just 5
      }

theWingedSerpentTheFuryOfYig :: CardDef
theWingedSerpentTheFuryOfYig =
  doubleSided "53046"
    $ (enemy "53046b" ("The Winged Serpent" <:> "The Fury of Yig") PillarsOfJudgement 1)
      { cdCardTraits = setFromList [Monster, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

featheredSerpent :: CardDef
featheredSerpent =
  (enemy "53047" "Feathered Serpent" ReturnToPillarsOfJudgement 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Serpent]
    , cdVengeancePoints = Just 1
    }

captiveSubjects :: CardDef
captiveSubjects =
  (enemy "53058" "Captive Subjects" ReturnToTheCityOfArchives 2)
    { cdHealth = health 5
    , cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }

brotherhoodAcolyte :: CardDef
brotherhoodAcolyte =
  (enemy "53071" "Brotherhood Acolyte" CultOfPnakotus 3)
    { cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

stolenMind :: CardDef
stolenMind =
  (enemy "53072" "Stolen Mind" CultOfPnakotus 1)
    { cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

tindalosAlpha :: CardDef
tindalosAlpha =
  (enemy "53077" "Tindalos Alpha" TemporalHunters 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    }

vengefulSerpent :: CardDef
vengefulSerpent =
  (enemy "53078" "Vengeful Serpent" VenomousHate 3)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVengeancePoints = Just 0
    }

serpentGuardian :: CardDef
serpentGuardian =
  (enemy "53079" "Serpent Guardian" VenomousHate 1)
    { cdHealth = health 5
    , cdCardTraits = setFromList [Construct, Serpent]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

senatorNathanielRhodesAdeptPolitician :: CardDef
senatorNathanielRhodesAdeptPolitician =
  (enemy "54025" ("Senator Nathanial Rhodes" <:> "Adept Politician") ReturnToAtDeathsDoorstep 1)
    { cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }

dmitriKonstantinovTakingTheLongView :: CardDef
dmitriKonstantinovTakingTheLongView =
  (enemy "54026" ("Dmitri Konstantinov" <:> "Taking the Long View") ReturnToAtDeathsDoorstep 1)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    }

returnToHeretic_38 :: CardDef
returnToHeretic_38 =
  doubleSided "54038b"
    $ (enemy "54038" "Heretic" ReturnToTheWagesOfSin 1)
      { cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

returnToHeretic_39 :: CardDef
returnToHeretic_39 =
  doubleSided "54039b"
    $ (enemy "54039" "Heretic" ReturnToTheWagesOfSin 1)
      { cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Geist, Witch, Spectral, Elite]
      }

screechingBanshee :: CardDef
screechingBanshee =
  (enemy "54074" "Screeching Banshee" BloodthirstySpirits 2)
    { cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Geist, Spectral]
    , cdKeywords = singleton Keyword.Hunter
    }
