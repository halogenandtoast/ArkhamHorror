module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.FogOverInnsmouth where

import Arkham.Treachery.CardDefs.Import

fogOverInnsmouth :: CardDef
fogOverInnsmouth =
  (treachery "07095" "Fog over Innsmouth" FogOverInnsmouth 2)
    { cdCardTraits = singleton Hazard
    }
