module Arkham.Enemy.CardDefs.TracesToNowhere where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

abductorThatWhichLurksInShadows :: CardDef
abductorThatWhichLurksInShadows =
  unique
    $ doubleSided "73053b"
    $ (enemy "73053" ("Abductor" <:> "That Which Lurks in Shadows") DimensionalMadness 1)
      { cdArt = "73053a"
      , cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = singleton Elite
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

albrechtHusserlDesperateGambler :: CardDef
albrechtHusserlDesperateGambler =
  unique
    $ doubleSided "73019b"
    $ (enemy "73019" ("Albrecht Husserl" <:> "Desperate Gambler") TracesToNowhere 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Culprit, Elite]
      , cdVictoryPoints = Just 0
      }

ambushingShadow :: CardDef
ambushingShadow =
  (enemy "73054" "Ambushing Shadow" DimensionalMadness 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = singleton Monster
    }

bruiser :: CardDef
bruiser =
  (enemy "73038" "Bruiser" TracesToNowhere 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Syndicate]
    , cdKeywords = singleton Keyword.Hunter
    }

crookedCops :: CardDef
crookedCops =
  (enemy "73022" "Crooked Cops" TracesToNowhere 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Police, Syndicate]
    , cdVictoryPoints = Just 1
    , cdRevelation = IsRevelation
    }

dimensionalShamblerDraggingThemBelow :: CardDef
dimensionalShamblerDraggingThemBelow =
  unique
    $ doubleSided "73053"
    $ (enemy "73053b" ("Dimensional Shambler" <:> "Dragging Them Below") DimensionalMadness 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 10
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords =
          setFromList
            [Keyword.Retaliate, Keyword.Patrol (LocationWithMostCardsUnderneath Anywhere)]
      , cdVictoryPoints = Just 2
      }

eugenePeelDoubleCrossingCultist :: CardDef
eugenePeelDoubleCrossingCultist =
  unique
    $ doubleSided "73020b"
    $ (enemy "73020" ("Eugene Peel" <:> "Double-Crossing Cultist") TracesToNowhere 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Culprit, Elite]
      , cdVictoryPoints = Just 0
      }

hiredEnforcer :: CardDef
hiredEnforcer =
  (enemy "73039" "Hired Enforcer" TracesToNowhere 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Syndicate]
    , cdKeywords = singleton Keyword.Retaliate
    }

linaHusserlVisionsOfHerOwnDemise :: CardDef
linaHusserlVisionsOfHerOwnDemise =
  unique
    $ doubleSided "73021b"
    $ (enemy "73021" ("Lina Husserl" <:> "Visions of Her Own Demise") TracesToNowhere 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Culprit, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 0
      }

theCleanerLeaveItToHim :: CardDef
theCleanerLeaveItToHim =
  unique
    $ (enemy "73040" ("The Cleaner" <:> "Leave It to Him") TracesToNowhere 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Syndicate, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol LocationWithAnyClues]
      , cdVictoryPoints = Just 1
      }

thoseWhichGrasp :: CardDef
thoseWhichGrasp =
  (enemy "73055" "Those Which Grasp" DimensionalMadness 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    }
