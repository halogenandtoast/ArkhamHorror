module Arkham.Treachery.CardDefs.ReturnToNightOfTheZealot.ReturnToTheMidnightMasks where

import Arkham.Treachery.CardDefs.Import

maskedHorrors :: CardDef
maskedHorrors =
  (treachery "50031" "Masked Horrors" ReturnToTheMidnightMasks 2)
    { cdCardTraits = setFromList [Power, Scheme]
    }
