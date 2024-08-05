module Arkham.Helpers.Matchers where

import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

matchChaosToken
  :: HasGame m => InvestigatorId -> ChaosToken -> Matcher.ChaosTokenMatcher -> m Bool
matchChaosToken _ = (<=~>)
