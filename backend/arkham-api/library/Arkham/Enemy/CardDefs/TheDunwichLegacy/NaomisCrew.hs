module Arkham.Enemy.CardDefs.TheDunwichLegacy.NaomisCrew where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mobster :: CardDef
mobster =
  (enemy "02098" "Mobster" NaomisCrew 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

oBannionsThug :: CardDef
oBannionsThug =
  (enemy "02097" "O'Bannion's Thug" NaomisCrew 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    }
