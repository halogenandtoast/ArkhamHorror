module Arkham.Enemy.CardDefs.TheLabyrinthsOfLunacy where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

eixodolon :: CardDef
eixodolon =
  unique
    $ (enemy "70048" ("Eixodolon" <:> "Your Anguish Is My Power") TheLabyrinthsOfLunacy 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Humanoid, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 3
      }

eixodolonsPet :: CardDef
eixodolonsPet =
  unique
    $ ( enemy
          "70050"
          ("Eixodolon's Pet" <:> "You Are Not Its First Victim, Or Its Last")
          LabyrinthsOfLunacySingleGroup
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

eixodolonsPetEpicMultiplayer :: CardDef
eixodolonsPetEpicMultiplayer =
  unique
    $ ( enemy
          "70049"
          ("Eixodolon's Pet" <:> "You Are Not Its First Victim, Or Its Last")
          LabyrinthsOfLunacyEpicMultiplayer
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

facelessAbductor :: CardDef
facelessAbductor =
  (enemy "70052" "Faceless Abductor" TheLabyrinthsOfLunacy 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

miGoGuard :: CardDef
miGoGuard =
  (enemy "70054" "Mi-Go Guard" TheLabyrinthsOfLunacy 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, MiGo]
    }

theJailor :: CardDef
theJailor =
  unique
    $ (enemy "70051" "The Jailor" LabyrinthsOfLunacyEpicMultiplayer 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 12
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 5
      }

torturedVictim :: CardDef
torturedVictim =
  (enemy "70053" "Tortured Victim" TheLabyrinthsOfLunacy 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Extradimensional]
    , cdRevelation = IsRevelation
    }
