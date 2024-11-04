module Arkham.Scenarios.TheEssexCountyExpress.Helpers where

import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Direction
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude

leftmostLocation :: HasGame m => m LocationId
leftmostLocation = go =<< selectJust (LocationWithTitle "Engine Car")
 where
  go lid = maybe (pure lid) go =<< selectOne (LocationInDirection LeftOf $ LocationWithId lid)
