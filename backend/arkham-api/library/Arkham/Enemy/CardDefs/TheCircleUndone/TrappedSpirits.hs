module Arkham.Enemy.CardDefs.TheCircleUndone.TrappedSpirits where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

wraith :: CardDef
wraith =
  (enemy "05103" "Wraith" TrappedSpirits 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist, Spectral]
    , cdKeywords = singleton Keyword.Hunter
    }
