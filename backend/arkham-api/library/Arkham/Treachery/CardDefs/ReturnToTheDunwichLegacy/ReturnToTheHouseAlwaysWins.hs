module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.ReturnToTheHouseAlwaysWins where

import Arkham.Treachery.CardDefs.Import

caughtCheating :: CardDef
caughtCheating =
  surge
    $ (treachery "51018" "Caught Cheating" ReturnToTheHouseAlwaysWins 2)
      { cdCardTraits = setFromList [Illicit]
      }

raiseTheStakes :: CardDef
raiseTheStakes =
  (treachery "51019" "Raise the Stakes" ReturnToTheHouseAlwaysWins 2)
    { cdCardTraits = setFromList [Illicit]
    }
