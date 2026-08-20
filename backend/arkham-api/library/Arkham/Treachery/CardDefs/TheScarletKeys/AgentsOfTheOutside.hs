module Arkham.Treachery.CardDefs.TheScarletKeys.AgentsOfTheOutside where

import Arkham.Treachery.CardDefs.Import

matterInversion :: CardDef
matterInversion =
  (treachery "09738" "Matter Inversion" AgentsOfTheOutside 2)
    { cdCardTraits = setFromList [Power]
    }
