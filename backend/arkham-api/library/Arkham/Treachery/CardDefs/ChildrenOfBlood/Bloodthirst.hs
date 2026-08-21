module Arkham.Treachery.CardDefs.ChildrenOfBlood.Bloodthirst where

import Arkham.Treachery.CardDefs.Import

bloodthirst :: CardDef
bloodthirst =
  (treachery "13102" "Bloodthirst" Bloodthirst 3)
    { cdCardTraits = singleton Madness
    }
