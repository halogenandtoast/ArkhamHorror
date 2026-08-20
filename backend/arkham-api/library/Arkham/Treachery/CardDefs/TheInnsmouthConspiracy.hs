module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy where

import Arkham.Treachery.CardDefs.Import

crisisOfFaith :: CardDef
crisisOfFaith =
  (weakness "07007" "Crisis of Faith")
    { cdCardTraits = singleton Madness
    }

sirenCall :: CardDef
sirenCall =
  (weakness "07016" "Siren Call")
    { cdCardTraits = singleton Curse
    }

dreadCurse :: CardDef
dreadCurse =
  (basicWeakness "07039" "Dread Curse")
    { cdCardTraits = singleton Curse
    }

dayOfReckoning :: CardDef
dayOfReckoning =
  (basicWeakness "07040" "Day of Reckoning")
    { cdCardTraits = singleton Endtimes
    }
