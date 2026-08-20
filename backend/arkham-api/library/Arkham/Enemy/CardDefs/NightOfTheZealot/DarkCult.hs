module Arkham.Enemy.CardDefs.NightOfTheZealot.DarkCult where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

acolyte :: CardDef
acolyte =
  (enemy "01169" "Acolyte" DarkCult 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

wizardOfTheOrder :: CardDef
wizardOfTheOrder =
  (enemy "01170" "Wizard of the Order" DarkCult 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }
