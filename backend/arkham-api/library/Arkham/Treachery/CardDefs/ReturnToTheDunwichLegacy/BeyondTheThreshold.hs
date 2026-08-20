module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.BeyondTheThreshold where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

aBalefulWelcome :: CardDef
aBalefulWelcome =
  (treachery "51062" "A Baleful Welcome" BeyondTheThreshold 2)
    { cdCardTraits = setFromList [Hex]
    , cdKeywords = setFromList [Keyword.Peril]
    }

hauntingRecollections :: CardDef
hauntingRecollections =
  (treachery "51061" "Haunting Recollections" BeyondTheThreshold 2)
    { cdCardTraits = setFromList [Hex]
    }

infiniteDoorway :: CardDef
infiniteDoorway =
  (treachery "51063" "Infinite Doorway" BeyondTheThreshold 2)
    { cdCardTraits = setFromList [Hex]
    }
