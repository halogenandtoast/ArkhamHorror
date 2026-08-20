module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.RisingTide where

import Arkham.Treachery.CardDefs.Import

riptide :: CardDef
riptide =
  (treachery "07093" "Riptide" RisingTide 2)
    { cdCardTraits = singleton Hazard
    }

risingTides :: CardDef
risingTides =
  (treachery "07092" "Rising Tides" RisingTide 2)
    { cdCardTraits = singleton Hazard
    }

undertow :: CardDef
undertow =
  (treachery "07091" "Undertow" RisingTide 2)
    { cdCardTraits = singleton Hazard
    }
