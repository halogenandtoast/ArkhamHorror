module Arkham.Enemy.CardDefs.TheDrownedCity.OneLastJob where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

gangEnforcer :: CardDef
gangEnforcer =
  (enemy "11513" "Gang Enforcer" OneLastJob 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

gangInformant :: CardDef
gangInformant =
  (enemy "11514" "Gang Informant" OneLastJob 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

gangSoldier :: CardDef
gangSoldier =
  (enemy "11512" "Gang Soldier" OneLastJob 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }

naomiOBannion :: CardDef
naomiOBannion =
  unique
    $ (enemy "11511" ("Naomi O'Bannion" <:> "Just Doing Business") OneLastJob 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      , cdVictoryPoints = Just 1
      }

sadieSheldon :: CardDef
sadieSheldon =
  unique
    $ (enemy "11510" ("Sadie Sheldon" <:> "Runs this Town") OneLastJob 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }
