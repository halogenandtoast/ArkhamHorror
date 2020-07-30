module Arkham.Types.PlayerTreachery
  ( allPlayerTreacheries
  )
where

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import ClassyPrelude

-- What to do when a player draws a treachery

allPlayerTreacheries
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => CardCode
  -> InvestigatorId
  -> m ()
allPlayerTreacheries "01007" = coverUp
allPlayerTreacheries ptid =
  const (throwString $ "No event with id: " <> show ptid)

coverUp
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
coverUp iid = unshiftMessage (DrewTreachery iid "01007")
