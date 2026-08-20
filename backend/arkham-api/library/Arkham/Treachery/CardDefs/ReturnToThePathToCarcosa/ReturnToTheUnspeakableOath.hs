module Arkham.Treachery.CardDefs.ReturnToThePathToCarcosa.ReturnToTheUnspeakableOath where

import Arkham.Treachery.CardDefs.Import

cloudedMemory :: CardDef
cloudedMemory =
  peril
    (treachery "52039" "Clouded Memory" ReturnToTheUnspeakableOath 1)
      { cdCardTraits = setFromList [Terror]
      }

radicalTreatment :: CardDef
radicalTreatment =
  (treachery "52038" "Radical Treatment" ReturnToTheUnspeakableOath 1)
    { cdVictoryPoints = Just 1
    , cdRevelation = NoRevelation
    }
