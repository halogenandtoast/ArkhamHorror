module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.Mutations where

import Arkham.Treachery.CardDefs.Import

suddenMutation :: CardDef
suddenMutation =
  (treachery "10741" "Sudden Mutation" Mutations 2)
    { cdCardTraits = setFromList [Power, Colour]
    }

unnaturalGrowth :: CardDef
unnaturalGrowth =
  (treachery "10742" "Unnatural Growth" Mutations 3)
    { cdCardTraits = singleton Curse
    }
