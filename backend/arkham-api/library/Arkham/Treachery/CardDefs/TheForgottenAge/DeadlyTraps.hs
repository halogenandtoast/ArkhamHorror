module Arkham.Treachery.CardDefs.TheForgottenAge.DeadlyTraps where

import Arkham.Treachery.CardDefs.Import

entombed :: CardDef
entombed =
  (treachery "04089" "Entombed" DeadlyTraps 2)
    { cdCardTraits = singleton Trap
    }

finalMistake :: CardDef
finalMistake =
  (treachery "04088" "Final Mistake" DeadlyTraps 3)
    { cdCardTraits = singleton Trap
    }
