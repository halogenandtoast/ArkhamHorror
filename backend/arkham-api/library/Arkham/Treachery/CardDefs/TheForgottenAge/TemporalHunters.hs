module Arkham.Treachery.CardDefs.TheForgottenAge.TemporalHunters where

import Arkham.Treachery.CardDefs.Import

mergingTimelines :: CardDef
mergingTimelines =
  (treachery "53076" "Merging Timelines" TemporalHunters 3)
    { cdCardTraits = setFromList [Hex]
    }
