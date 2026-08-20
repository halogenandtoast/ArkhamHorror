module Arkham.Enemy.CardDefs.TheForgottenAge.CultOfPnakotus where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

brotherhoodAcolyte :: CardDef
brotherhoodAcolyte =
  (enemy "53071" "Brotherhood Acolyte" CultOfPnakotus 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

stolenMind :: CardDef
stolenMind =
  (enemy "53072" "Stolen Mind" CultOfPnakotus 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }
