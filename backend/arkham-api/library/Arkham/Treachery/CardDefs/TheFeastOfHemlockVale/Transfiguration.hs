module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.Transfiguration where

import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

fungalRot :: CardDef
fungalRot =
  (treachery "10727" "Fungal Rot" Transfiguration 2)
    { cdCardTraits = setFromList [Hazard, Trait.Blight]
    }

strangeMutations :: CardDef
strangeMutations =
  (treachery "10726" "Strange Mutations" Transfiguration 2)
    { cdCardTraits = setFromList [Power]
    }
