module Arkham.Treachery.CardDefs.NightOfTheZealot.ChillingCold where

import Arkham.Treachery.CardDefs.Import

cryptChill :: CardDef
cryptChill =
  (treachery "01167" "Crypt Chill" ChillingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }

obscuringFog :: CardDef
obscuringFog =
  (treachery "01168" "Obscuring Fog" ChillingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }
