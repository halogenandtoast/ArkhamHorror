module Arkham.Treachery.CardDefs.NightOfTheZealot.LockedDoors where

import Arkham.Treachery.CardDefs.Import

lockedDoor :: CardDef
lockedDoor =
  (treachery "01174" "Locked Door" LockedDoors 2)
    { cdCardTraits = setFromList [Obstacle]
    }
