module Arkham.Treachery.CardDefs.TheDreamEaters.DreamersCurse where

import Arkham.Treachery.CardDefs.Import

deeperSlumber :: CardDef
deeperSlumber =
  (treachery "06095" "Deeper Slumber" DreamersCurse 2)
    { cdCardTraits = singleton Curse
    }

dreamersCurse :: CardDef
dreamersCurse =
  (treachery "06093" "Dreamer's Curse" DreamersCurse 2)
    { cdCardTraits = singleton Curse
    }

somniphobia :: CardDef
somniphobia =
  (treachery "06094" "Somniphobia" DreamersCurse 2)
    { cdCardTraits = singleton Terror
    }
