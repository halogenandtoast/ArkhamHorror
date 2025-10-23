module Arkham.Helpers.Action where

import Arkham.Action
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher.Action
import Arkham.Prelude
import Arkham.Tracing

actionMatches :: (Tracing m, HasGame m) => InvestigatorId -> Action -> ActionMatcher -> m Bool
