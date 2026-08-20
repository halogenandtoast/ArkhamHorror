module Arkham.Treachery.CardDefs.TheForgottenAge.ShatteredAeons where

import Arkham.Treachery.CardDefs.Import

betweenWorlds :: CardDef
betweenWorlds =
  (treachery "04340" "Between Worlds" ShatteredAeons 2)
    { cdCardTraits = singleton Hex
    }

creepingDarkness :: CardDef
creepingDarkness =
  (treachery "04342" "Creeping Darkness" ShatteredAeons 2)
    { cdCardTraits = singleton Hazard
    }

shatteredAges :: CardDef
shatteredAges =
  (treachery "04339" "Shattered Ages" ShatteredAeons 2)
    { cdCardTraits = singleton Hex
    }

wrackedByTime :: CardDef
wrackedByTime =
  (treachery "04341" "Wracked by Time" ShatteredAeons 3)
    { cdCardTraits = singleton Hex
    }
