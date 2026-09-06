{- HLINT ignore "Use camelCase" -}
module Arkham.Treachery.CardDefs.EdgeOfTheEarth where

import Arkham.Treachery.CardDefs.Import

theHarbinger :: CardDef
theHarbinger =
  signature "08004"
    $ (weakness "08006" "The Harbinger")
      { cdCardTraits = setFromList [Omen, Endtimes]
      , cdOutOfPlayEffects = [OnTopOfDeckEffect]
      }

buriedSecrets :: CardDef
buriedSecrets =
  signature "08007"
    $ (weakness "08009" "Buried Secrets")
      { cdCardTraits = setFromList [Mystery]
      }

burdenOfDestiny :: CardDef
burdenOfDestiny =
  signature "08010"
    $ (weakness "08015" "Burden of Destiny")
      { cdCardTraits = setFromList [Flaw]
      }

greed :: CardDef
greed =
  signature "08016"
    $ (weakness "08018" "Greed")
      { cdCardTraits = setFromList [Flaw]
      }

armInjury :: CardDef
armInjury =
  (basicWeakness "08130" "Arm Injury")
    { cdCardTraits = singleton Injury
    }

legInjury :: CardDef
legInjury =
  (basicWeakness "08131" "Leg Injury")
    { cdCardTraits = singleton Injury
    }

panic :: CardDef
panic =
  (basicWeakness "08132" "Panic")
    { cdCardTraits = singleton Madness
    }

stupor :: CardDef
stupor =
  (basicWeakness "08133" "Stupor")
    { cdCardTraits = singleton Madness
    }
