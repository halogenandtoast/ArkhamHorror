module Arkham.Treachery.CardDefs.TheDreamEaters.Corsairs where

import Arkham.Treachery.CardDefs.Import

huntedByCorsairs :: CardDef
huntedByCorsairs =
  (treachery "06104" "Hunted by Corsairs" Corsairs 2)
    { cdCardTraits = singleton Scheme
    }
