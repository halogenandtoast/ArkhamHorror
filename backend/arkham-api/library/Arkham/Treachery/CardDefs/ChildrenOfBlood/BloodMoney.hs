module Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoney where

import Arkham.Treachery.CardDefs.Import

sanguineRebirth :: CardDef
sanguineRebirth =
  (treachery "13092" "Sanguine Rebirth" BloodMoney 2)
    { cdCardTraits = singleton Scheme
    }
