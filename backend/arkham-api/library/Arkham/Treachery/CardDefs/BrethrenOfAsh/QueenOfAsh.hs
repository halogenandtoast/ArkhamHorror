module Arkham.Treachery.CardDefs.BrethrenOfAsh.QueenOfAsh where

import Arkham.Treachery.CardDefs.Import

ashenRebirth :: CardDef
ashenRebirth =
  (treachery "12176" "Ashen Rebirth" QueenOfAsh 2)
    { cdCardTraits = singleton Power
    }

blasphemousInvocation :: CardDef
blasphemousInvocation =
  (treachery "12190" "Blasphemous Invocation" Cultists 2)
    { cdCardTraits = singleton Hex
    }

dissonance :: CardDef
dissonance =
  (treachery "12194" "Dissonance" Torment 2)
    { cdCardTraits = setFromList [Curse, Bane]
    }

languor :: CardDef
languor =
  (treachery "12193" "Languor" Torment 2)
    { cdCardTraits = setFromList [Hex, Bane]
    }

putridVapors :: CardDef
putridVapors =
  (treachery "12192" "Putrid Vapors" ReekingDecay 2)
    { cdCardTraits = singleton Hazard
    }

torment :: CardDef
torment =
  (peril $ treachery "12195" "Torment" Torment 2)
    { cdCardTraits = setFromList [Power, Bane]
    }

unnaturalDecay :: CardDef
unnaturalDecay =
  (treachery "12191" "Unnatural Decay" ReekingDecay 2)
    { cdCardTraits = singleton Curse
    }
