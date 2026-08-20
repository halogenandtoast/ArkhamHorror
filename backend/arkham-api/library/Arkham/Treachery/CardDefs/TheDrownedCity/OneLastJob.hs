module Arkham.Treachery.CardDefs.TheDrownedCity.OneLastJob where

import Arkham.Treachery.CardDefs.Import

caughtInTheCrossfire :: CardDef
caughtInTheCrossfire =
  (treachery "11515" "Caught in the Crossfire" OneLastJob 2) {cdCardTraits = setFromList [Hazard]}

endOfNegotiations :: CardDef
endOfNegotiations =
  (treachery "11516" "End of Negotiations" OneLastJob 2) {cdCardTraits = setFromList [Blunder]}
