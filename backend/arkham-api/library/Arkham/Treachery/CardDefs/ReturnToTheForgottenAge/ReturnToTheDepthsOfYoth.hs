module Arkham.Treachery.CardDefs.ReturnToTheForgottenAge.ReturnToTheDepthsOfYoth where

import Arkham.Treachery.CardDefs.Import

perilsOfYoth :: CardDef
perilsOfYoth =
  peril
    (treachery "53060" "Perils of Yoth" ReturnToTheDepthsOfYoth 1)
      { cdCardTraits = setFromList [Hazard]
      }
