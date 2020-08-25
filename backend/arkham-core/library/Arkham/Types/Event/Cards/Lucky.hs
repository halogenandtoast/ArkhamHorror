module Arkham.Types.Event.Cards.Lucky where

import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Modifier
import ClassyPrelude

lucky
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
lucky _ = unshiftMessage
  (AddModifier AfterSkillTestTarget (AnySkillValue 2 (EventSource "01080")))
