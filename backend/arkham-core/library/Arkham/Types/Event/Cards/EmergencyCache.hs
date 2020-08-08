module Arkham.Types.Event.Cards.EmergencyCache where

import Arkham.Types.InvestigatorId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.GameRunner
import ClassyPrelude

emergencyCache
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
emergencyCache iid = unshiftMessage (TakeResources iid 3 False)
