module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.Fire where

import Arkham.Treachery.CardDefs.Import

fire :: CardDef
fire =
  (treachery "10743" "Fire!" Fire 5)
    { cdCardTraits = singleton Hazard
    }
