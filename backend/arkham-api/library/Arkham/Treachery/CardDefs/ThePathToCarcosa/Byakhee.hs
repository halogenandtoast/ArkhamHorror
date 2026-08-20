module Arkham.Treachery.CardDefs.ThePathToCarcosa.Byakhee where

import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Treachery.CardDefs.Import

huntedByByakhee :: CardDef
huntedByByakhee =
  (treachery "03087" "Hunted by Byakhee" EncounterSet.Byakhee 2)
    { cdCardTraits = singleton Pact
    }
