module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheSecondDay where

import Arkham.Treachery.CardDefs.Import

downpour :: CardDef
downpour =
  (treachery "10678" "Downpour" TheSecondDay 3)
    { cdCardTraits = setFromList [Hazard]
    }
