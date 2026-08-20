module Arkham.Enemy.CardDefs.TheBlobThatAteEverything where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cubicOoze :: CardDef
cubicOoze =
  (enemy "85041" "Cubic Ooze" TheBlobThatAteEverything 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 1
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords = setFromList [Keyword.ScenarioKeywordX "Blob" 2]
    , cdRevelation = IsRevelation
    }

graspingOoze :: CardDef
graspingOoze =
  (enemy "85040" "Grasping Ooze" TheBlobThatAteEverything 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.ScenarioKeywordX "Blob" 3]
    }

miGoAbductor :: CardDef
miGoAbductor =
  (enemy "85036" "Mi-Go Abductor" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdVictoryPoints = Just 1
    }

miGoDestroyer :: CardDef
miGoDestroyer =
  (enemy "89013" "Mi-Go Destroyer" MiGoIncursionII 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    }

miGoDrone :: CardDef
miGoDrone =
  (enemy "85033" "Mi-Go Drone" MiGoIncursion 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, MiGo]
    , cdKeywords = setFromList [Keyword.Surge]
    }

miGoGeneral :: CardDef
miGoGeneral =
  (enemy "85027" "Mi-Go General" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Servitor, MiGo, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

miGoHarvester :: CardDef
miGoHarvester =
  (enemy "85034" "Mi-Go Harvester" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdVictoryPoints = Just 1
    }

miGoMeddler :: CardDef
miGoMeddler =
  (enemy "85035" "Mi-Go Meddler" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdVictoryPoints = Just 1
    }

miGoResearcher :: CardDef
miGoResearcher =
  (enemy "89018" "Mi-Go Researcher" MiGoIncursionII 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

miGoScientist :: CardDef
miGoScientist =
  (enemy "89015" "Mi-Go Scientist" MiGoIncursionII 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    }

oozeling :: CardDef
oozeling =
  (enemy "85039" "Oozeling" TheBlobThatAteEverything 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords = setFromList [Keyword.ScenarioKeywordX "Blob" 1]
    }

oozewraith :: CardDef
oozewraith =
  (enemy "85042" "Oozewraith" TheBlobThatAteEverything 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords =
        setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate, Keyword.ScenarioKeywordX "Blob" 5]
    , cdVictoryPoints = Just 1
    }

subject8L08 :: CardDef
subject8L08 =
  (enemy "85038" "Subject 8L-08" BlobSingleGroup 1)
    { cdHealth = healthPerInvestigator 15
    , cdCardTraits = setFromList [Monster, Ooze, Elite]
    }

subject8L08EpicMultiplayer :: CardDef
subject8L08EpicMultiplayer =
  (enemy "85037" "Subject 8L-08" BlobEpicMultiplayer 1)
    { cdHealth = healthStar
    , cdCardTraits = setFromList [Monster, Ooze, Elite]
    }

vulnerableHeart :: CardDef
vulnerableHeart =
  (enemy "85043" "Vulnerable Heart" TheBlobThatAteEverything 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fightX
    , cdCardTraits = setFromList [Monster, Ooze, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    }
