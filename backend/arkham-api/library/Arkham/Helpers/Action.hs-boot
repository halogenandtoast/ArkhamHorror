module Arkham.Helpers.Action where

import Arkham.Action
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher.Action
import Arkham.Prelude

actionMatches :: HasGame m => InvestigatorId -> Action -> ActionMatcher -> m Bool
