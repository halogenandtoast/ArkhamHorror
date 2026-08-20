module Arkham.Treachery.CardDefs.ReturnToTheDunwichLegacy.YogSothothsEmissaries where

import Arkham.Treachery.CardDefs.Import

eldritchAccord :: CardDef
eldritchAccord =
  peril
    $ (treachery "51072" "Eldritch Accord" YogSothothsEmissaries 2)
      { cdCardTraits = setFromList [Pact]
      }
