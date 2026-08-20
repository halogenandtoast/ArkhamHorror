module Arkham.Treachery.CardDefs.BrethrenOfAsh.SmokeAndMirrors where

import Arkham.Treachery.CardDefs.Import

arcaneLock :: CardDef
arcaneLock =
  (treachery "12157" "Arcane Lock" ArcaneLock 2)
    { cdCardTraits = setFromList [Hex, Obstacle]
    }

crossfire :: CardDef
crossfire =
  (treachery "12165" "Crossfire" GangsOfArkham 2)
    { cdCardTraits = singleton Hazard
    }

downpour :: CardDef
downpour =
  (treachery "12158" "Downpour" BadWeather 2)
    { cdCardTraits = singleton Hazard
    }

eagerForDeath2 :: CardDef
eagerForDeath2 =
  (treachery "12167" "Eager for Death" Whippoorwills2 2)
    { cdCardTraits = setFromList [Omen]
    }

flashFlood :: CardDef
flashFlood =
  (treachery "12159" "Flash Flood" BadWeather 2)
    { cdCardTraits = singleton Hazard
    }

markOfElokoss :: CardDef
markOfElokoss =
  (weakness "12137" "Mark of Elokoss")
    { cdCardTraits = singleton Curse
    , cdEncounterSet = Just SmokeAndMirrors
    , cdEncounterSetQuantity = Just 4
    }
