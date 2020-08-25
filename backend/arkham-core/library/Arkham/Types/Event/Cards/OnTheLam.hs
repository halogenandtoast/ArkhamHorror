module Arkham.Types.Event.Cards.OnTheLam where

import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

onTheLam
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
onTheLam iid = unshiftMessages
  [ AddModifier
      (InvestigatorTarget iid)
      (CannotBeAttackedByNonElite (EventSource "01010"))
  ]
