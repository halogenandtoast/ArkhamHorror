module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.TheLocals where

import Arkham.Treachery.CardDefs.Import

furtiveLocals :: CardDef
furtiveLocals =
  (treachery "07107" "Furtive Locals" TheLocals 2)
    { cdCardTraits = singleton Terror
    }

innsmouthLook :: CardDef
innsmouthLook =
  (treachery "07106" "Innsmouth Look" TheLocals 2)
    { cdCardTraits = setFromList [Curse, Terror]
    }
