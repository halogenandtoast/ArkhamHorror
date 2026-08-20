module Arkham.Enemy.CardDefs.TheForgottenAge.GuardiansOfTime where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

eztliGuardian :: CardDef
eztliGuardian =
  (enemy "04086" "Eztli Guardian" GuardiansOfTime 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Eztli]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
    }
