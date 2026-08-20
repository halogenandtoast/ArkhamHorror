module Arkham.Treachery.CardDefs.ThePathToCarcosa.CultOfTheYellowSign where

import Arkham.Treachery.CardDefs.Import

theKingsEdict :: CardDef
theKingsEdict =
  (treachery "03100" "The King's Edict" CultOfTheYellowSign 2)
    { cdCardTraits = singleton Pact
    }
