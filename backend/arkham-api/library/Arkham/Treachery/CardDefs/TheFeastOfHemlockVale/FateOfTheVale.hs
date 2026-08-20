module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.FateOfTheVale where

import Arkham.Treachery.CardDefs.Import

euphoria :: CardDef
euphoria =
  (treachery "10674" "Euphoria" FateOfTheVale 2)
    { cdCardTraits = singleton Terror
    }

fragmentation :: CardDef
fragmentation =
  (treachery "10673" "Fragmentation" FateOfTheVale 2)
    { cdCardTraits = singleton Power
    }

sublimation :: CardDef
sublimation =
  (treachery "10672" "Sublimation" FateOfTheVale 2)
    { cdCardTraits = singleton Power
    }
