module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheLostSister where

import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

luminousGrowth :: CardDef
luminousGrowth =
  (treachery "10587" "Luminous Growth" TheLostSister 3)
    { cdCardTraits = setFromList [Hazard, Trait.Flora]
    }

reclaimedByNature :: CardDef
reclaimedByNature =
  (treachery "10586" "Reclaimed by Nature" TheLostSister 3)
    { cdCardTraits = setFromList [Hazard]
    }
