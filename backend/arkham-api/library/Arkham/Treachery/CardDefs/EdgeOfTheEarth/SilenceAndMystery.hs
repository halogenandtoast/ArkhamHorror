module Arkham.Treachery.CardDefs.EdgeOfTheEarth.SilenceAndMystery where

import Arkham.Treachery.CardDefs.Import

darkAurora :: CardDef
darkAurora =
  (treachery "08713" "Dark Aurora" SilenceAndMystery 3)
    { cdCardTraits = setFromList [Terror]
    }

polarMirage :: CardDef
polarMirage =
  (treachery "08712" "Polar Mirage" SilenceAndMystery 2)
    { cdCardTraits = setFromList [Terror]
    }
