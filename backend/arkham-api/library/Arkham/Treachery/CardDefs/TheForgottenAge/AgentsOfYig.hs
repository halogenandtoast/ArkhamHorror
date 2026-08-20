module Arkham.Treachery.CardDefs.TheForgottenAge.AgentsOfYig where

import Arkham.Treachery.CardDefs.Import

curseOfYig :: CardDef
curseOfYig =
  (treachery "04085" "Curse of Yig" AgentsOfYig 2)
    { cdCardTraits = singleton Curse
    }
