module Arkham.Enemy.CardDefs.TheCircleUndone.AnettesCoven where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

covenInitiate :: CardDef
covenInitiate =
  (enemy "05090" "Coven Initiate" AnettesCoven 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdRevelation = IsRevelation
    }

priestessOfTheCoven :: CardDef
priestessOfTheCoven =
  (enemy "05091" "Priestess of the Coven" AnettesCoven 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = singleton Keyword.Retaliate
    }
