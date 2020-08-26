module Arkham.Types.Event.Cards.DrawnToTheFlame where

import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import ClassyPrelude

drawnToTheFlame
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
drawnToTheFlame iid = do
  lid <- asks (getId iid)
  unshiftMessages
    [InvestigatorDrawEncounterCard iid, InvestigatorDiscoverClues iid lid 2]

