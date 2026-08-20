module Arkham.Treachery.CardDefs.TheScarletKeys where

import Arkham.Treachery.CardDefs.Import

selflessToAFault :: CardDef
selflessToAFault =
  (weakness "09003" "Selfless to a Fault")
    { cdCardTraits = setFromList [Flaw]
    }

deafeningSilence :: CardDef
deafeningSilence =
  (weakness "09014" "Deafening Silence")
    { cdCardTraits = setFromList [Omen]
    }

ruinedFilm :: CardDef
ruinedFilm =
  (weakness "09017" "Ruined Film")
    { cdCardTraits = setFromList [Blunder]
    }

burdenOfLeadership :: CardDef
burdenOfLeadership =
  (weakness "09020" "Burden of Leadership")
    { cdCardTraits = setFromList [Flaw]
    }
