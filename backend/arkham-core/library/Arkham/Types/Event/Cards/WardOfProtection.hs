module Arkham.Types.Event.Cards.WardOfProtection where

import Arkham.Types.InvestigatorId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.GameRunner
import Arkham.Types.Source
import ClassyPrelude

wardOfProtection
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
wardOfProtection iid = unshiftMessages
  [CancelNextRevelationEffect, InvestigatorDamage iid (EventSource "01065") 0 1]

