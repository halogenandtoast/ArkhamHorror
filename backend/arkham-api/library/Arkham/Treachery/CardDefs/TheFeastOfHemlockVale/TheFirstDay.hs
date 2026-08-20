module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheFirstDay where

import Arkham.Treachery.CardDefs.Import

swarm :: CardDef
swarm =
  (treachery "10676" "Swarm" TheFirstDay 3)
    { cdCardTraits = setFromList [Hazard]
    }
