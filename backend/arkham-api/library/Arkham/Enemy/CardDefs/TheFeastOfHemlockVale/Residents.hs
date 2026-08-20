module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.Residents where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

bertieMusgrave :: CardDef
bertieMusgrave =
  unique
    $ doubleSided "10701"
    $ (enemy "10701b" "Bertie Musgrave" Residents 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Miskatonic]
      , cdKeywords =
          setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithEnemy (EnemyWithTitle "Mother Rachel"))]
      , cdVictoryPoints = Just 0
      }

gideonMizrah :: CardDef
gideonMizrah =
  unique
    $ doubleSided "10698"
    $ (enemy "10698b" "Gideon Mizrah" Residents 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 1
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

judithPark :: CardDef
judithPark =
  unique
    $ doubleSided "10699"
    $ (enemy "10699b" "Judith Park" Residents 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

leahAtwood :: CardDef
leahAtwood =
  unique
    $ doubleSided "10694"
    $ (enemy "10694b" "Leah Atwood" Residents 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

motherRachelStarbornHerald :: CardDef
motherRachelStarbornHerald =
  unique
    $ doubleSided "10693"
    $ (enemy "10693b" "Mother Rachel" Residents 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

riverHawthorne :: CardDef
riverHawthorne =
  unique
    $ doubleSided "10697"
    $ (enemy "10697b" "River Hawthorne" Residents 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

simeonAtwood :: CardDef
simeonAtwood =
  unique
    $ doubleSided "10695"
    $ (enemy "10695b" "Simeon Atwood" Residents 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 1
      , cdEvade = evade 4
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

theoPeters :: CardDef
theoPeters =
  unique
    $ doubleSided "10700"
    $ (enemy "10700b" "Theo Peters" Residents 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Alert]
      , cdVictoryPoints = Just 0
      }

williamHemlock :: CardDef
williamHemlock =
  unique
    $ doubleSided "10696"
    $ (enemy "10696b" "William Hemlock" Residents 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Resident, Elite]
      , cdKeywords = setFromList [Keyword.Elusive]
      , cdVictoryPoints = Just 0
      }
