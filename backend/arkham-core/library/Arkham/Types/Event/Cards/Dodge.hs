module Arkham.Types.Event.Cards.Dodge where

import Arkham.Types.InvestigatorId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.GameRunner
import ClassyPrelude

dodge
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
dodge _ = unshiftMessage CancelNextAttack
