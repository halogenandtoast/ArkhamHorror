module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheTwistedHollow where

import Arkham.Treachery.CardDefs.Import

deepShadows :: CardDef
deepShadows =
  (treachery "10622" "Deep Shadows" TheTwistedHollow 2)
    { cdCardTraits = setFromList [Hazard]
    }

lurkingFear :: CardDef
lurkingFear =
  (treachery "10623" "Lurking Fear" TheTwistedHollow 2)
    { cdCardTraits = setFromList [Terror]
    }

stolenLight :: CardDef
stolenLight =
  peril
    $ (treachery "10624" "Stolen Light" TheTwistedHollow 1)
      { cdCardTraits = setFromList [Scheme]
      }
