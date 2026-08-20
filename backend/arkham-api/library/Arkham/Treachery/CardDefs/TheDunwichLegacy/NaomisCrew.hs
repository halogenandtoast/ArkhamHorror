module Arkham.Treachery.CardDefs.TheDunwichLegacy.NaomisCrew where

import Arkham.Treachery.CardDefs.Import

huntedDown :: CardDef
huntedDown =
  (treachery "02099" "Hunted Down" NaomisCrew 2)
    { cdCardTraits = setFromList [Tactic]
    }
