module Arkham.Types.Event.Cards.DarkMemory where

import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import ClassyPrelude

darkMemory
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
darkMemory _ =
  unshiftMessages [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
