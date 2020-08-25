module Arkham.Types.PlayerRevelation
  ( allPlayerRevelations
  )
where

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import ClassyPrelude

-- What to do when a player draws a treachery

allPlayerRevelations
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => CardCode
  -> InvestigatorId
  -> CardId
  -> m ()
allPlayerRevelations "01007" = putIntoPlay -- cover up
allPlayerRevelations "01009" = putIntoPlay -- the necronomicon
allPlayerRevelations "01015" = putIntoPlay -- abandoned and alone
allPlayerRevelations ptid =
  const (const (throwString $ "No relation for card with id: " <> show ptid))

putIntoPlay
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => InvestigatorId
  -> CardId
  -> m ()
putIntoPlay iid cardId = unshiftMessage (PlayCard iid cardId False)
