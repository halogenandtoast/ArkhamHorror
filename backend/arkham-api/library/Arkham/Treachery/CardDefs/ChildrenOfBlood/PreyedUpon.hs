module Arkham.Treachery.CardDefs.ChildrenOfBlood.PreyedUpon where

import Arkham.Treachery.CardDefs.Import

feedingGrounds :: CardDef
feedingGrounds =
  (treachery "13111" "Feeding Grounds" PreyedUpon 2)
    { cdCardTraits = singleton Hazard
    }
