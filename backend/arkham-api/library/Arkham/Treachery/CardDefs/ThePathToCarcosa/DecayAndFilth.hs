module Arkham.Treachery.CardDefs.ThePathToCarcosa.DecayAndFilth where

import Arkham.Treachery.CardDefs.Import

corrosion :: CardDef
corrosion =
  (treachery "03102" "Corrosion" DecayAndFilth 2)
    { cdCardTraits = singleton Hazard
    }

oozeAndFilth :: CardDef
oozeAndFilth =
  (treachery "03101" "Ooze and Filth" DecayAndFilth 2)
    { cdCardTraits = singleton Hazard
    }
