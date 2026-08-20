module Arkham.Enemy.CardDefs.ReturnToTheDunwichLegacy.ReturnToExtracurricularActivities where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

enthralledSecurityGuard :: CardDef
enthralledSecurityGuard =
  (enemy "51014" "Entralled Security Guard" ReturnToExtracurricularActivities 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }
