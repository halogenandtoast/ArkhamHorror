module Arkham.Enemy.CardDefs.NightOfTheZealot.TheGathering where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

fleshEater :: CardDef
fleshEater =
  (enemy "01118" "Flesh-Eater" TheGathering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdVictoryPoints = Just 1
    }

ghoulPriest :: CardDef
ghoulPriest =
  (enemy "01116" "Ghoul Priest" TheGathering 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

icyGhoul :: CardDef
icyGhoul =
  (enemy "01119" "Icy Ghoul" TheGathering 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdVictoryPoints = Just 1
    }
