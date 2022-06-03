module Arkham.Scenarios.TheEssexCountyExpress.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Direction
import Arkham.Matcher
import Arkham.Id

leftmostLocation :: Query LocationMatcher m => LocationId -> m LocationId
leftmostLocation lid = do
  mlid' <- selectOne $ LocationInDirection LeftOf $ LocationWithId lid
  maybe (pure lid) leftmostLocation mlid'
