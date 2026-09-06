module Arkham.Treachery.CardDefs.TheScarletKeys where

import Arkham.Treachery.CardDefs.Import

selflessToAFault :: CardDef
selflessToAFault =
  signature "09001"
    $ (weakness "09003" "Selfless to a Fault")
      { cdCardTraits = setFromList [Flaw]
      }

deafeningSilence :: CardDef
deafeningSilence =
  signature "09011"
    $ (weakness "09014" "Deafening Silence")
      { cdCardTraits = setFromList [Omen]
      }

ruinedFilm :: CardDef
ruinedFilm =
  signature "09015"
    $ (weakness "09017" "Ruined Film")
      { cdCardTraits = setFromList [Blunder]
      }

burdenOfLeadership :: CardDef
burdenOfLeadership =
  signature "09018"
    $ (weakness "09020" "Burden of Leadership")
      { cdCardTraits = setFromList [Flaw]
      }
