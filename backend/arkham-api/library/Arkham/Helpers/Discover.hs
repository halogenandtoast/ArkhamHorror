module Arkham.Helpers.Discover where

import Arkham.Classes.HasGame
import Arkham.Discover
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing

getDiscoverLocation :: (HasGame m, Tracing m) => InvestigatorId -> Discover -> m (Maybe LocationId)
getDiscoverLocation iid d = case d.location of
  DiscoverAtLocation lid' -> pure (Just lid')
  DiscoverYourLocation -> field InvestigatorLocation iid
