module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheLocals where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

innsmouthTroublemaker :: CardDef
innsmouthTroublemaker =
  (enemy "07105" "Innsmouth Troublemaker" TheLocals 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Hybrid, Criminal]
    , cdKeywords = singleton Keyword.Hunter
    }
