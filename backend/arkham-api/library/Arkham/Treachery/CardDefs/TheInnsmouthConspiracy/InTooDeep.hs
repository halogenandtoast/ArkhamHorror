module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy.InTooDeep where

import Arkham.Treachery.CardDefs.Import

deepOneInvasion :: CardDef
deepOneInvasion =
  (treachery "07147" "Deep One Invasion" InTooDeep 1)
    { cdCardTraits = singleton Scheme
    }

inundated :: CardDef
inundated =
  (treachery "07149" "Inundated" InTooDeep 3)
    { cdCardTraits = singleton Hazard
    }

pulledBack :: CardDef
pulledBack =
  (treachery "07148" "Pulled Back" InTooDeep 2)
    { cdCardTraits = singleton Terror
    }
