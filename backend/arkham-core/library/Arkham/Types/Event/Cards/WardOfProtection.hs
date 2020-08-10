module Arkham.Types.Event.Cards.WardOfProtection where

import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import ClassyPrelude

wardOfProtection
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
wardOfProtection iid = unshiftMessages
  [ CancelNext RevelationMessage
  , InvestigatorAssignDamage iid (EventSource "01065") 0 1
  ]

