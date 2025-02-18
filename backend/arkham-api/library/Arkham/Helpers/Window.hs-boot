module Arkham.Helpers.Window where

import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Source
import Arkham.Window

windowMatches
  :: (HasGame m, HasCallStack)
  => InvestigatorId
  -> Source
  -> Window
  -> Matcher.WindowMatcher
  -> m Bool
