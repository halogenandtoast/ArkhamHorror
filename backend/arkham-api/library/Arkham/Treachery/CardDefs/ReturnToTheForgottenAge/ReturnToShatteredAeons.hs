module Arkham.Treachery.CardDefs.ReturnToTheForgottenAge.ReturnToShatteredAeons where

import Arkham.Treachery.CardDefs.Import

unknowablePast :: CardDef
unknowablePast =
  peril
    (treachery "53065" "Unknowable Past" ReturnToShatteredAeons 2)
      { cdCardTraits = setFromList [Hex]
      }
