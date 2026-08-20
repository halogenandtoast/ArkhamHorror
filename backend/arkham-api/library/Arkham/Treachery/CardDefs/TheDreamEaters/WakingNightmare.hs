module Arkham.Treachery.CardDefs.TheDreamEaters.WakingNightmare where

import Arkham.Treachery.CardDefs.Import

outbreak :: CardDef
outbreak =
  (treachery "06083" "Outbreak" WakingNightmare 3)
    { cdCardTraits = singleton Hazard
    }
