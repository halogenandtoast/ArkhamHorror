module Arkham.Treachery.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte where

import Arkham.Treachery.CardDefs.Import

unnaturalStrength :: CardDef
unnaturalStrength =
  (treachery "13098" "Unnatural Strength" AgentsOfZburamoarte 2)
    { cdCardTraits = singleton Power
    }
