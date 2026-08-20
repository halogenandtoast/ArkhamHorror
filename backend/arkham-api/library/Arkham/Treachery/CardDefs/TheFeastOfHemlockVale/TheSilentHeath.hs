module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.TheSilentHeath where

import Arkham.Treachery.CardDefs.Import

commandingResonance :: CardDef
commandingResonance =
  (treachery "10561" "Commanding Resonance" TheSilentHeath 2)
    { cdCardTraits = setFromList [Scheme]
    }

defendTheNest :: CardDef
defendTheNest =
  (treachery "10562" "Defend the Nest" TheSilentHeath 2)
    { cdCardTraits = setFromList [Scheme]
    }
