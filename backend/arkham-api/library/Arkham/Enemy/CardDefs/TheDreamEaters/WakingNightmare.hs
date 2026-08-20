module Arkham.Enemy.CardDefs.TheDreamEaters.WakingNightmare where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

corruptedOrderly :: CardDef
corruptedOrderly =
  (enemy "06082" "Corrupted Orderly" WakingNightmare 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Staff, Spider]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

suspiciousOrderly :: CardDef
suspiciousOrderly =
  (enemy "06081" "Suspicious Orderly" WakingNightmare 2)
    { cdEvade = evade 2
    , cdCardTraits = setFromList [Humanoid, Staff]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
