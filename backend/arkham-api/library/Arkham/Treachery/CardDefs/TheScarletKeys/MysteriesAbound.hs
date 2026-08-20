module Arkham.Treachery.CardDefs.TheScarletKeys.MysteriesAbound where

import Arkham.Treachery.CardDefs.Import

inPlainSight :: CardDef
inPlainSight =
  (treachery "09721" "In Plain Sight" MysteriesAbound 2)
    { cdCardTraits = setFromList [Scheme]
    }
