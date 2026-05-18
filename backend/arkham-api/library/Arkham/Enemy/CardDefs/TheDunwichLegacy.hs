module Arkham.Enemy.CardDefs.TheDunwichLegacy where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theExperiment :: CardDef
theExperiment =
  unique
    $ ( enemy
          "02058"
          ("The Experiment" <:> "Something Went Terribly Wrong")
          ExtracurricularActivity
          1
      )
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      , cdVictoryPoints = Just 2
      }

cloverClubPitBoss :: CardDef
cloverClubPitBoss =
  (enemy "02078" "Clover Club Pit Boss" TheHouseAlwaysWins 1)
    { cdCardTraits = setFromList [Humanoid, Criminal, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

thrall :: CardDef
thrall =
  (enemy "02086" "Thrall" BishopsThralls 3)
    { cdCardTraits = setFromList [Humanoid, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

wizardOfYogSothoth :: CardDef
wizardOfYogSothoth =
  (enemy "02087" "Wizard of Yog-Sothoth" BishopsThralls 1)
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

whippoorwill :: CardDef
whippoorwill =
  (enemy "02090" "Whippoorwill" Whippoorwills 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

avianThrall :: CardDef
avianThrall =
  (enemy "02094" "Avian Thrall" BeastThralls 2)
    { cdCardTraits = setFromList [Creature, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

lupineThrall :: CardDef
lupineThrall =
  (enemy "02095" "Lupine Thrall" BeastThralls 2)
    { cdCardTraits = setFromList [Creature, Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

oBannionsThug :: CardDef
oBannionsThug =
  (enemy "02097" "O'Bannion's Thug" NaomisCrew 2)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }

mobster :: CardDef
mobster =
  (enemy "02098" "Mobster" NaomisCrew 2)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

conglomerationOfSpheres :: CardDef
conglomerationOfSpheres =
  (enemy "02103" "Conglomeration of Spheres" HideousAbominations 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

servantOfTheLurker :: CardDef
servantOfTheLurker =
  (enemy "02104" "Servant of the Lurker" HideousAbominations 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

huntingHorror :: CardDef
huntingHorror =
  ( enemy
      "02141"
      ("Hunting Horror" <:> "Spawned from the Void")
      TheMiskatonicMuseum
      1
  )
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

grapplingHorror :: CardDef
grapplingHorror =
  (enemy "02182" "Grappling Horror" TheEssexCountyExpress 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

emergentMonstrosity :: CardDef
emergentMonstrosity =
  (enemy "02183" "Emergent Monstrosity" TheEssexCountyExpress 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdVictoryPoints = Just 1
    }

silasBishop :: CardDef
silasBishop =
  unique
    $ (enemy "02216" ("Silas Bishop" <:> "Infused With Evil") BloodOnTheAltar 1)
      { cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = singleton Keyword.Massive
      , cdVictoryPoints = Just 2
      }

servantOfManyMouths :: CardDef
servantOfManyMouths =
  (enemy "02224" "Servant of Many Mouths" BloodOnTheAltar 3)
    { cdCardTraits = singleton Humanoid
    , cdKeywords = singleton Keyword.Retaliate
    }

broodOfYogSothoth :: CardDef
broodOfYogSothoth =
  (enemy "02255" "Brood of Yog-Sothoth" UndimensionedAndUnseen 5)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

sethBishop :: CardDef
sethBishop =
  unique
    $ (enemy "02293" ("Seth Bishop" <:> "Sorcerer of Dunwich") WhereDoomAwaits 1)
      { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

devoteeOfTheKey :: CardDef
devoteeOfTheKey =
  (enemy "02294" "Devotee of the Key" WhereDoomAwaits 2)
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    }

crazedShoggoth :: CardDef
crazedShoggoth =
  (enemy "02295" "Crazed Shoggoth" WhereDoomAwaits 1)
    { cdCardTraits = setFromList [Monster, Shoggoth]
    , cdVictoryPoints = Just 1
    }

yogSothoth :: CardDef
yogSothoth =
  unique
    $ ( enemy
          "02323"
          ("Yog-Sothoth" <:> "The Lurker Beyond the Threshold")
          LostInTimeAndSpace
          1
      )
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords =
          setFromList
            [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
      }

interstellarTraveler :: CardDef
interstellarTraveler =
  (enemy "02329" "Interstellar Traveler" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

yithianStarseeker :: CardDef
yithianStarseeker =
  (enemy "02330" "Yithian Starseeker" LostInTimeAndSpace 2)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }
