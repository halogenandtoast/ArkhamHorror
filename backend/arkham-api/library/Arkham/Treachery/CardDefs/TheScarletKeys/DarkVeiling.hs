module Arkham.Treachery.CardDefs.TheScarletKeys.DarkVeiling where

import Arkham.Treachery.CardDefs.Import

figuresInTheDark :: CardDef
figuresInTheDark =
  peril
    (treachery "09725" "Figures in the Dark" DarkVeiling 2)
      { cdCardTraits = setFromList [Scheme]
      }

seeingShadows :: CardDef
seeingShadows =
  (treachery "09724" "Seeing Shadows" DarkVeiling 2)
    { cdCardTraits = setFromList [Terror]
    }
