module Arkham.Enemy.CardDefs.TheDunwichLegacy.TheHouseAlwaysWins where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cloverClubPitBoss :: CardDef
cloverClubPitBoss =
  (enemy "02078" "Clover Club Pit Boss" TheHouseAlwaysWins 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Criminal, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
