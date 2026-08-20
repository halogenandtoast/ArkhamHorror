module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.ReturnToTheMiskatonicMuseum where

import Arkham.Treachery.CardDefs.Import

darkBidding :: CardDef
darkBidding =
  (treachery "51023" "Dark Bidding" ReturnToTheMiskatonicMuseum 2)
    { cdCardTraits = setFromList [Power]
    }

nightBeyondVoid :: CardDef
nightBeyondVoid =
  (treachery "51024" "Night Beyond Void" ReturnToTheMiskatonicMuseum 2)
    { cdCardTraits = setFromList [Power]
    , cdVictoryPoints = Just 0
    }
