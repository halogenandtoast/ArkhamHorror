module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.HorrorsInTheRock where

import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

calcification :: CardDef
calcification =
  (treachery "10723" "Calcification" HorrorsInTheRock 2)
    { cdCardTraits = setFromList [Hazard, Trait.Blight]
    }

chromaBlight :: CardDef
chromaBlight =
  (treachery "10722" "Chroma Blight" HorrorsInTheRock 2)
    { cdCardTraits = setFromList [Power, Trait.Blight]
    }
