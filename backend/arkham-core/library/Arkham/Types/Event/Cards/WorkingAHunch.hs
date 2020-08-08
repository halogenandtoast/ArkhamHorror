module Arkham.Types.Event.Cards.WorkingAHunch where

import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Classes
import Arkham.Types.Query
import Arkham.Types.Message
import Arkham.Types.GameRunner
import ClassyPrelude

workingAHunch
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
workingAHunch iid = do
  currentLocationId <- asks (getId @LocationId iid)
  clueCount <- unClueCount <$> asks (getCount currentLocationId)
  if clueCount > 0
    then unshiftMessage (DiscoverCluesAtLocation iid currentLocationId 1)
    else pure ()

