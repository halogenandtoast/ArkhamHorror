module Arkham.Treachery.CardDefs.TheForgottenAge.DoomedExpedition where

import Arkham.Treachery.CardDefs.Import

bestLaidPlans :: CardDef
bestLaidPlans =
  peril
    (treachery "53075" "Best-Laid Plans" DoomedExpedition 3)
      { cdCardTraits = setFromList [Blunder]
      }

resentfulWilds :: CardDef
resentfulWilds =
  (treachery "53074" "Resentful Wilds" DoomedExpedition 2)
    { cdCardTraits = setFromList [Hazard]
    , cdVengeancePoints = Just 1
    }
