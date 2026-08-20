module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.Blight where

import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

desiccation :: CardDef
desiccation =
  (treachery "10729" "Desiccation" EncounterSet.Blight 2)
    { cdCardTraits = setFromList [Trait.Blight]
    }

enervation :: CardDef
enervation =
  (treachery "10728" "Enervation" EncounterSet.Blight 2)
    { cdCardTraits = setFromList [Hazard, Trait.Blight]
    }
